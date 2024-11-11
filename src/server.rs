use std::{collections::HashMap, path::PathBuf};

use anyhow::{bail, Result};
use log::info;
use lsp_types::{TextDocumentContentChangeEvent, Uri};
use ropey::Rope;
use tempdir::TempDir;

use crate::{
    file::SourceFile,
    package_index::{get_package_index, installed_packages, PackageIndex, Symbol},
    symbol_index::FileSymbol,
    utils::parse_url,
    workspace::{self, MultiFile, SingleFile, WorkSpace},
};

pub struct Server {
    workspaces: HashMap<String, WorkSpace>,
    package_index: PackageIndex,
    installed_packages: HashMap<String, PathBuf>,
    temp_dir: TempDir,
}

impl Server {
    pub fn new(
        workspaces: HashMap<String, WorkSpace>,
        package_index: PackageIndex,
        installed_packages: HashMap<String, PathBuf>,
        temp_dir: TempDir,
    ) -> Self {
        Self {
            package_index,
            workspaces,
            installed_packages,
            temp_dir,
        }
    }

    pub fn initialize(params: lsp_types::InitializeParams) -> Result<Self> {
        let workspaces_folders = params.workspace_folders;

        let mut workspaces = HashMap::new();

        info!("Initializing workspaces");

        #[allow(deprecated)]
        if let Some(folders) = workspaces_folders {
            for folder in folders {
                if let Some(workspace) = MultiFile::create(folder.uri)? {
                    workspaces.insert(folder.name, WorkSpace::MultiFile(workspace));
                }
            }
        } else if let Some(root_path) = params.root_uri {
            if let Some(workspace) = MultiFile::create(root_path.clone())? {
                workspaces.insert(root_path.to_string(), WorkSpace::MultiFile(workspace));
            }
        }

        info!("Creating package index");
        let package_index = get_package_index(workspaces.values().filter_map(|x| x.description()))?;

        info!("Getting installed packages");
        let installed_packages = installed_packages()?;

        info!("Creating tempdir");
        let temp_dir = TempDir::new("r-analyzer")?;

        Ok(Self::new(
            workspaces,
            package_index,
            installed_packages,
            temp_dir,
        ))
    }

    pub fn add_file(&mut self, uri: &lsp_types::Uri, text: Rope) -> anyhow::Result<()> {
        let path = parse_url(uri)?;
        let workspace = self
            .workspaces
            .values_mut()
            .find(|x| path.starts_with(x.path()));

        match workspace {
            Some(WorkSpace::SingleFile(_)) => panic!("Redefinition of existing file"),
            Some(WorkSpace::MultiFile(x)) => {
                x.add_file(path, text)?;
            }
            None => {
                self.workspaces.insert(
                    uri.to_string(),
                    WorkSpace::SingleFile(SingleFile::create(path, text)?),
                );
            }
        }

        Ok(())
    }

    pub fn update_file(
        &mut self,
        path: lsp_types::Uri,
        changes: Vec<TextDocumentContentChangeEvent>,
    ) -> Result<()> {
        let path = parse_url(&path)?;

        let workspace = self
            .workspaces
            .values_mut()
            .find(|workspace| path.starts_with(workspace.path()))
            .ok_or_else(|| anyhow::anyhow!("Workspace not found"))?;

        match workspace {
            WorkSpace::SingleFile(x) => x.file_mut().update(changes),
            WorkSpace::MultiFile(x) => {
                x.update_file(path, changes)?;
            }
        }

        Ok(())
    }

    pub fn remove_file(&mut self, uri: &lsp_types::Uri) -> anyhow::Result<()> {
        let path = parse_url(uri)?;

        let (name, workspace) = self
            .workspaces
            .iter_mut()
            .find(|(_, workspace)| path.starts_with(workspace.path()))
            .ok_or_else(|| anyhow::anyhow!("Workspace not found"))?;

        match workspace {
            WorkSpace::SingleFile(_) => {
                let name = name.clone();
                self.workspaces.remove(&name);
            }
            WorkSpace::MultiFile(x) => {
                x.remove_file(&path)?;
            }
        }

        Ok(())
    }

    pub fn file_context(&self, uri: &lsp_types::Uri) -> Result<FileContext> {
        let path = parse_url(uri)?;
        let workspace = match self
            .workspaces
            .values()
            .find(|x| path.starts_with(x.path()))
        {
            Some(x) => x,
            None => bail!("Workspace not found"),
        };

        let file = match workspace {
            WorkSpace::SingleFile(x) => x.file(),
            WorkSpace::MultiFile(x) => match x.get_file(&path) {
                Some(x) => x,
                None => bail!("File not found"),
            },
        };

        Ok(FileContext::new(uri.clone(), path, workspace, file))
    }

    pub fn source_file(&self, uri: &lsp_types::Uri) -> Result<&SourceFile> {
        let path = parse_url(uri)?;
        let workspace = match self
            .workspaces
            .values()
            .find(|x| path.starts_with(x.path()))
        {
            Some(x) => x,
            None => bail!("Workspace not found"),
        };

        workspace
            .get_file(&path)
            .ok_or_else(|| anyhow::anyhow!("File not found"))
    }

    pub fn installed_packages(&self) -> &HashMap<String, PathBuf> {
        &self.installed_packages
    }

    pub fn temp_dir(&self) -> &TempDir {
        &self.temp_dir
    }

    pub fn package_index(&self) -> &PackageIndex {
        &self.package_index
    }
}

pub struct FileContext<'a> {
    uri: Uri,
    file: PathBuf,
    workspace: &'a WorkSpace,
    source_file: &'a SourceFile,
}

impl<'a> FileContext<'a> {
    fn new(uri: Uri, file: PathBuf, workspace: &'a WorkSpace, source_file: &'a SourceFile) -> Self {
        Self {
            uri,
            file,
            workspace,
            source_file,
        }
    }

    pub fn uri(&self) -> &Uri {
        &self.uri
    }

    pub fn file(&self) -> &PathBuf {
        &self.file
    }

    pub fn workspace(&self) -> &WorkSpace {
        self.workspace
    }

    pub fn source_file(&self) -> &SourceFile {
        self.source_file
    }

    pub fn get_file(&self, path: &PathBuf) -> Option<&SourceFile> {
        self.workspace.get_file(path)
    }

    pub fn find_symbol(&self, name: &str) -> Option<&FileSymbol> {
        match self.workspace {
            WorkSpace::SingleFile(x) => x.index().find_symbol(name),
            WorkSpace::MultiFile(x) => x.symbol_index().find_symbol(name),
        }
    }
}
