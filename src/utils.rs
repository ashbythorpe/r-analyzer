use camino::Utf8PathBuf;
use url::Url;

pub fn parse_url(uri: lsp_types::Uri) -> anyhow::Result<Utf8PathBuf> {
    let url = Url::parse(uri.as_str())?;

    let path = match url.to_file_path() {
        Ok(x) => x,
        Err(_) => {
            return Err(anyhow::anyhow!("Invalid file path: {:?}", url));
        }
    };

    Ok(camino::absolute_utf8(&path)?)
}
