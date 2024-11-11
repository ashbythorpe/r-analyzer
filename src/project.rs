pub struct WorkSpace {
    pub files: HashMap<PathBuf, SourceFile>,
    root_dir: Option<PathBuf>,
    description: Option<DescriptionFile>,
    package_index: PackageIndex,
    symbol_index: SymbolIndex,
    installed_packages: HashMap<String, PathBuf>,
    temp_dir: TempDir,
}
