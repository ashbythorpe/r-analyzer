use std::{collections::HashMap, path::PathBuf};

use anyhow::Result;
use camino::Utf8PathBuf;
use lsp_types::TextDocumentContentChangeEvent;
use ropey::Rope;

use crate::{
    description::{find_description, DescriptionFile},
    file::SourceFile,
    package_index::{get_package_index, installed_packages, PackageIndex},
    utils::parse_url,
};

pub struct Server {
    files: HashMap<Utf8PathBuf, SourceFile>,
    description: Option<DescriptionFile>,
    package_index: PackageIndex,
    installed_packages: HashMap<String, PathBuf>,
}

impl Server {
    pub fn new(
        description: Option<DescriptionFile>,
        package_index: PackageIndex,
        installed_packages: HashMap<String, PathBuf>,
    ) -> Self {
        Self {
            files: HashMap::new(),
            description,
            package_index,
            installed_packages,
        }
    }

    pub fn initialize(params: lsp_types::InitializeParams) -> Result<Self> {
        #[allow(deprecated)]
        let description = if let Some(root_path) = params.root_uri {
            find_description(parse_url(root_path)?)?
        } else {
            None
        };

        let package_index = get_package_index(&description)?;

        let installed_packages = installed_packages()?;

        Ok(Self::new(description, package_index, installed_packages))
    }

    pub fn add_file(&mut self, uri: lsp_types::Uri, text: Rope) -> anyhow::Result<()> {
        let path = parse_url(uri)?;
        self.files.insert(path, SourceFile::parse(text));

        Ok(())
    }

    pub fn get_file(&self, path: lsp_types::Uri) -> anyhow::Result<&SourceFile> {
        let path = parse_url(path)?;

        self.files
            .get(&path)
            .ok_or_else(|| anyhow::anyhow!("File does not exist"))
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

        Ok(())
    }

    pub fn remove_file(&mut self, uri: lsp_types::Uri) -> anyhow::Result<()> {
        let path = parse_url(uri)?;
        self.files.remove(&path);

        Ok(())
    }
}
