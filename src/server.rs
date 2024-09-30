use std::collections::HashMap;

use anyhow::Result;
use camino::Utf8PathBuf;
use lsp_types::TextDocumentContentChangeEvent;
use ropey::Rope;
use url::Url;

use crate::file::SourceFile;

pub struct Server {
    files: HashMap<Utf8PathBuf, SourceFile>,
}

impl Server {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
        }
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
        path: Utf8PathBuf,
        changes: Vec<TextDocumentContentChangeEvent>,
    ) -> Result<()> {
        let file = self
            .files
            .get_mut(&path)
            .ok_or_else(|| anyhow::anyhow!("File does not exist"))?;

        file.update(changes);

        Ok(())
    }
}

fn parse_url(uri: lsp_types::Uri) -> anyhow::Result<Utf8PathBuf> {
    let url = Url::parse(uri.as_str())?;

    let path = match url.to_file_path() {
        Ok(x) => x,
        Err(_) => {
            return Err(anyhow::anyhow!("Invalid file path: {:?}", url));
        }
    };

    Ok(camino::absolute_utf8(&path)?)
}
