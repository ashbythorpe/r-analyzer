use std::{
    collections::{hash_map, HashMap},
    fs::File,
    iter,
    path::{Path, PathBuf},
};

use anyhow::{bail, Result};
use log::info;
use lsp_types::TextDocumentContentChangeEvent;
use ropey::Rope;

use crate::{
    description::{find_description, DescriptionFile},
    file::SourceFile,
    symbol_index::{FileIndex, SymbolIndex},
    utils::parse_url,
};

pub enum WorkSpace {
    SingleFile(SingleFile),
    MultiFile(MultiFile),
}

impl WorkSpace {
    pub fn path(&self) -> &PathBuf {
        match self {
            WorkSpace::SingleFile(x) => x.path(),
            WorkSpace::MultiFile(x) => x.path(),
        }
    }

    pub fn is_single_file(&self) -> bool {
        matches!(self, WorkSpace::SingleFile(_))
    }

    pub fn get_file(&self, path: &PathBuf) -> Option<&SourceFile> {
        match self {
            WorkSpace::SingleFile(x) => {
                if x.path() == path {
                    Some(x.file())
                } else {
                    None
                }
            }
            WorkSpace::MultiFile(x) => x.get_file(path),
        }
    }

    pub fn description(&self) -> Option<&DescriptionFile> {
        match self {
            WorkSpace::SingleFile(_) => None,
            WorkSpace::MultiFile(x) => Some(x.description()),
        }
    }

    pub fn files(&self) -> WorkSpaceFiles {
        match self {
            WorkSpace::SingleFile(x) => {
                WorkSpaceFiles::SingleFile(iter::once((x.path(), x.file())))
            }
            WorkSpace::MultiFile(x) => WorkSpaceFiles::MultiFile(x.files().iter()),
        }
    }
}

pub enum WorkSpaceFiles<'a> {
    SingleFile(iter::Once<(&'a PathBuf, &'a SourceFile)>),
    MultiFile(hash_map::Iter<'a, PathBuf, SourceFile>),
}

impl<'a> Iterator for WorkSpaceFiles<'a> {
    type Item = (&'a PathBuf, &'a SourceFile);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            WorkSpaceFiles::SingleFile(x) => x.next(),
            WorkSpaceFiles::MultiFile(x) => x.next(),
        }
    }
}

pub struct SingleFile {
    path: PathBuf,
    file: SourceFile,
    index: FileIndex,
}

impl SingleFile {
    pub fn new(path: PathBuf, file: SourceFile, index: FileIndex) -> Self {
        Self { path, file, index }
    }

    pub fn create(path: PathBuf, content: Rope) -> Result<Self> {
        info!("Parsing file");
        let parsed = SourceFile::parse(content);

        info!("Creating file index");
        let index = FileIndex::create(&path, &parsed)?;

        info!("Creating single file");
        Ok(Self::new(path, parsed, index))
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    pub fn file(&self) -> &SourceFile {
        &self.file
    }

    pub fn index(&self) -> &FileIndex {
        &self.index
    }

    pub fn file_mut(&mut self) -> &mut SourceFile {
        &mut self.file
    }
}

pub struct MultiFile {
    files: HashMap<PathBuf, SourceFile>,
    path: PathBuf,
    description: Box<DescriptionFile>,
    symbol_index: SymbolIndex,
}

impl MultiFile {
    pub fn new(
        files: HashMap<PathBuf, SourceFile>,
        path: PathBuf,
        description: DescriptionFile,
        symbol_index: SymbolIndex,
    ) -> Self {
        Self {
            files,
            path,
            description: Box::new(description),
            symbol_index,
        }
    }

    pub fn create(uri: lsp_types::Uri) -> Result<Option<Self>> {
        let path = parse_url(&uri)?;
        let description = match find_description(&path)? {
            Some(x) => x,
            None => return Ok(None),
        };
        let r_files = find_files(&path)?;

        let mut files = HashMap::new();

        for file in r_files {
            let content = Rope::from_reader(File::open(&file)?)?;
            let parsed = SourceFile::parse(content);
            files.insert(file, parsed);
        }

        let symbol_index = SymbolIndex::create(&files)?;

        Ok(Some(Self::new(files, path, description, symbol_index)))
    }

    pub fn add_file(&mut self, path: PathBuf, text: Rope) -> anyhow::Result<()> {
        let parsed = SourceFile::parse(text);
        self.file_insert(path, parsed)?;

        Ok(())
    }

    fn file_insert(&mut self, path: PathBuf, file: SourceFile) -> anyhow::Result<()> {
        self.symbol_index.add_file(path.clone(), &file)?;
        self.files.insert(path, file);

        Ok(())
    }

    pub fn get_file(&self, path: &PathBuf) -> Option<&SourceFile> {
        self.files.get(path)
    }

    pub fn get_path(&self, path: &PathBuf) -> Result<&SourceFile> {
        self.files
            .get(path)
            .ok_or_else(|| anyhow::anyhow!("File does not exist"))
    }

    pub fn update_file(
        &mut self,
        path: PathBuf,
        changes: Vec<TextDocumentContentChangeEvent>,
    ) -> Result<()> {
        let file = match self.files.get_mut(&path) {
            Some(x) => x,
            None => {
                bail!("File not found");
            }
        };

        file.update(changes);

        self.symbol_index.update_file(&path, file)?;

        Ok(())
    }

    pub fn remove_file(&mut self, path: &PathBuf) -> anyhow::Result<bool> {
        let result = self.files.remove(path).is_some();
        self.symbol_index.remove_file(path)?;

        Ok(result)
    }

    pub fn files(&self) -> &HashMap<PathBuf, SourceFile> {
        &self.files
    }

    pub fn description(&self) -> &DescriptionFile {
        &self.description
    }

    pub fn symbol_index(&self) -> &SymbolIndex {
        &self.symbol_index
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }
}

fn find_files(path: &Path) -> Result<Vec<PathBuf>> {
    let r_path = match path
        .read_dir()?
        .filter_map(|x| x.ok())
        .find(|x| x.file_type().is_ok_and(|t| t.is_dir()) && x.file_name() == "R")
        .map(|x| x.path())
    {
        Some(x) => x,
        None => return Ok(Vec::new()),
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
        .map(|x| x.path())
        .collect();

    Ok(files)
}
