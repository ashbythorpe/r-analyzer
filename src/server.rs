use std::{
    collections::{hash_map::Entry, HashMap},
    fs::File,
    path::PathBuf,
};

use anyhow::Result;
use camino::Utf8PathBuf;
use lsp_types::TextDocumentContentChangeEvent;
use ropey::Rope;
use tempdir::TempDir;

use crate::{
    description::{find_description, DescriptionFile},
    file::SourceFile,
    package_index::{get_package_index, installed_packages, PackageIndex},
    symbol_index::SymbolIndex,
    utils::parse_url,
};

pub struct Server {
    pub files: HashMap<Utf8PathBuf, SourceFile>,
    root_dir: Option<Utf8PathBuf>,
    description: Option<DescriptionFile>,
    package_index: PackageIndex,
    symbol_index: SymbolIndex,
    installed_packages: HashMap<String, PathBuf>,
    temp_dir: TempDir,
}

impl Server {
    pub fn new(
        description: Option<DescriptionFile>,
        root_dir: Option<Utf8PathBuf>,
        package_index: PackageIndex,
        symbol_index: SymbolIndex,
        installed_packages: HashMap<String, PathBuf>,
        temp_dir: TempDir,
    ) -> Self {
        Self {
            files: HashMap::new(),
            root_dir,
            description,
            package_index,
            symbol_index,
            installed_packages,
            temp_dir,
        }
    }

    pub fn initialize(params: lsp_types::InitializeParams) -> Result<Self> {
        #[allow(deprecated)]
        let (root_dir, description, r_files) = if let Some(root_path) = params.root_uri {
            let path = parse_url(root_path)?;
            (
                Some(path.clone()),
                find_description(&path)?,
                find_files(&path)?,
            )
        } else {
            (None, None, None)
        };

        let mut files = HashMap::new();

        if let Some(r_files) = r_files {
            for file in r_files {
                let text = Rope::from_reader(File::open(&file)?)?;
                let source = SourceFile::parse(text);
                files.insert(file, source);
            }
        }

        let symbol_index = SymbolIndex::create(&files)?;

        let package_index = get_package_index(&description)?;

        let installed_packages = installed_packages()?;

        let temp_dir = TempDir::new("r-analyzer")?;

        Ok(Self::new(
            description,
            root_dir,
            package_index,
            symbol_index,
            installed_packages,
            temp_dir,
        ))
    }

    pub fn add_file(&mut self, uri: lsp_types::Uri, text: Rope) -> anyhow::Result<()> {
        let path = parse_url(uri)?;
        let parsed = SourceFile::parse(text);
        self.symbol_index.add_file(path.clone(), &parsed)?;
        self.files.insert(path, parsed);

        Ok(())
    }

    pub fn get_file(&self, path: lsp_types::Uri) -> anyhow::Result<&SourceFile> {
        let path = parse_url(path)?;

        self.files
            .get(&path)
            .ok_or_else(|| anyhow::anyhow!("File does not exist"))
    }

    pub fn get_path(&self, path: Utf8PathBuf) -> Result<&SourceFile> {
        self.files
            .get(&path)
            .ok_or_else(|| anyhow::anyhow!("File does not exist"))
    }

    pub fn get_or_insert_file(&mut self, path: lsp_types::Uri) -> anyhow::Result<&mut SourceFile> {
        let path = parse_url(path)?;

        let entry = self.files.entry(path.clone());

        match entry {
            Entry::Occupied(x) => Ok(x.into_mut()),
            Entry::Vacant(x) => {
                let text = Rope::from_reader(File::open(&path)?)?;
                let parsed = SourceFile::parse(text);
                self.symbol_index.add_file(path.clone(), &parsed)?;
                Ok(x.insert(parsed))
            }
        }
    }

    pub fn update_file(
        &mut self,
        path: lsp_types::Uri,
        changes: Vec<TextDocumentContentChangeEvent>,
    ) -> Result<()> {
        let path = parse_url(path)?;

        let file = self
            .files
            .get_mut(&path)
            .ok_or_else(|| anyhow::anyhow!("File does not exist"))?;

        file.update(changes);

        self.symbol_index.update_file(&path, file)?;

        Ok(())
    }

    pub fn remove_file(&mut self, uri: lsp_types::Uri) -> anyhow::Result<()> {
        let path = parse_url(uri)?;
        self.files.remove(&path);
        self.symbol_index.remove_file(&path)?;

        Ok(())
    }

    pub fn description(&self) -> Option<&DescriptionFile> {
        self.description.as_ref()
    }

    pub fn package_index(&self) -> &PackageIndex {
        &self.package_index
    }

    pub fn installed_packages(&self) -> &HashMap<String, PathBuf> {
        &self.installed_packages
    }

    pub fn temp_dir(&self) -> &TempDir {
        &self.temp_dir
    }

    pub fn symbol_index(&self) -> &SymbolIndex {
        &self.symbol_index
    }

    pub fn root_dir(&self) -> Option<&Utf8PathBuf> {
        self.root_dir.as_ref()
    }
}

fn find_files(path: &Utf8PathBuf) -> Result<Option<Vec<Utf8PathBuf>>> {
    let r_path = match path
        .read_dir()?
        .filter_map(|x| x.ok())
        .find(|x| x.file_type().is_ok_and(|t| t.is_dir()) && x.file_name() == "R")
        .map(|x| x.path())
    {
        Some(x) => x,
        None => return Ok(None),
    };

    let files = r_path
        .read_dir()?
        .filter_map(|x| x.ok())
        .filter(|x| {
            x.file_type().is_ok_and(|x| x.is_file())
                && x.path()
                    .extension()
                    .is_some_and(|x| x.to_str() == Some("R"))
        })
        .filter_map(|x| Utf8PathBuf::from_path_buf(x.path()).ok())
        .collect();

    Ok(Some(files))
}
