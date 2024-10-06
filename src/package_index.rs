use std::collections::hash_map::Entry;
use std::str::Lines;
use std::{
    collections::HashMap,
    fs::{self, DirEntry},
    path::PathBuf,
    process::Command,
};

use anyhow::Result;

use fst::Map;

use crate::description::DescriptionFile;

// An R script to get every item in a package's index.
// Returns each item separated by two newlines in the format
//
// type exported name <NEWLINE> ?formals
//
// where type is defined as
//   0 = object
//   1 = function
//   2 = lazydata
//
// and exported is 1 if the item is exported and 0 otherwise
//
// formals is defined as:
// [name=?default<NEWLINE>]*
pub fn get_objects_r(name: &str) -> String {
    format!(
        "
ns <- asNamespace('{}')
exports <- getNamespaceExports(ns)
objects <- union(names(ns), exports)
is_exported <- as.integer(objects %in% exports)

for (i in seq_along(objects)) {{
    name <- objects[i]
    object <- get0(name, ns)
    is_function <- as.integer(is.function(object))
    cat(is_function, is_exported[i], name, '\\n', sep = '')

    if (is_function) {{
        formals <- formals(object)
        for (j in seq_along(formals)) {{
            cat(names(formals)[j], '=', deparse(formals[[j]]), sep = '')
            cat('\\n')
        }}

        cat('\\n')
    }}
}}

lazydata <- names(.getNamespaceInfo(ns, 'lazydata'))
for (item in lazydata) {{
    cat(2, 1, item, '\\n', sep = '')
}}
",
        name
    )
}

#[derive(Debug)]
pub struct PackageIndex {
    packages: HashMap<String, Package>,
}

#[derive(Debug)]
pub struct Package {
    name: String,
    exported: Vec<Symbol>,
    not_exported: Vec<Symbol>,
    export_map: Map<Vec<u8>>,
}

impl Package {
    fn new(
        name: String,
        exported: Vec<Symbol>,
        not_exported: Vec<Symbol>,
        export_map: Map<Vec<u8>>,
    ) -> Self {
        Self {
            name,
            exported,
            not_exported,
            export_map,
        }
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn exported(&self) -> &[Symbol] {
        &self.exported
    }

    fn not_exported(&self) -> &[Symbol] {
        &self.not_exported
    }
}

#[derive(Debug)]
pub enum Symbol {
    Function { name: String, signature: Vec<Arg> },
    Object { name: String },
}

impl Symbol {
    fn name(&self) -> &str {
        match self {
            Symbol::Function { name, .. } => name,
            Symbol::Object { name } => name,
        }
    }
}

#[derive(Debug)]
pub struct Arg {
    name: String,
    default: Option<String>,
}

impl Arg {
    fn name(&self) -> &str {
        &self.name
    }

    fn default(&self) -> Option<&String> {
        self.default.as_ref()
    }
}

pub fn get_package_index(description: &Option<DescriptionFile>) -> Result<PackageIndex> {
    let base_packages = vec![
        "base",
        "compiler",
        "datasets",
        "graphics",
        "grDevices",
        "grid",
        "methods",
        "parallel",
        "splines",
        "stats",
        "stats4",
        "tcltk",
        "tools",
        "utils",
    ];
    let recommended_packages = vec![
        "boot",
        "MASS",
        "Matrix",
        "survival",
        "boot",
        "class",
        "cluster",
        "codetools",
        "foreign",
        "KernSmooth",
        "lattice",
        "MASS",
        "Matrix",
        "mgcv",
        "nnet",
        "rpart",
        "spatial",
        "survival",
    ];

    let mut packages = HashMap::new();

    add_packages(&mut packages, &base_packages)?;

    add_packages(&mut packages, &recommended_packages)?;

    if let Some(description_file) = description {
        let description = &description_file.description;
        if let Some(x) = description.depends() {
            add_packages_string(&mut packages, x)?;
        }

        if let Some(x) = description.suggests() {
            add_packages_string(&mut packages, x)?;
        }

        if let Some(x) = description.imports() {
            add_packages_string(&mut packages, x)?;
        }

        if let Some(x) = description.enhances() {
            add_packages_string(&mut packages, x)?;
        }
    }

    Ok(PackageIndex { packages })
}

fn add_packages(packages: &mut HashMap<String, Package>, names: &Vec<&str>) -> Result<()> {
    for name in names {
        if let Entry::Vacant(x) = packages.entry(name.to_string()) {
            x.insert(get_package_symbols(name.to_string())?);
        }
    }

    Ok(())
}

fn add_packages_string(packages: &mut HashMap<String, Package>, names: &Vec<String>) -> Result<()> {
    for name in names {
        if let Entry::Vacant(x) = packages.entry(name.to_owned()) {
            x.insert(get_package_symbols(name.to_owned())?);
        }
    }

    Ok(())
}

fn get_symbol_map(symbols: &Vec<Symbol>) -> Result<Map<Vec<u8>>> {
    Ok(Map::from_iter(
        symbols
            .iter()
            .enumerate()
            .map(|(i, x)| (x.name().as_bytes(), i as u64)),
    )?)
}

fn create_entry(package_index: usize, symbol_index: usize) -> u64 {
    (package_index as u64) << 32 | (symbol_index as u32 as u64)
}

fn get_package_symbols(package: String) -> Result<Package> {
    let command_result = Command::new("R")
        .args(["--slave", "-e", get_objects_r(&package).as_str()])
        .output()?;

    if !command_result.status.success() {
        anyhow::bail!("Failed to get package index")
    }

    let output = String::from_utf8(command_result.stdout)?;
    let mut exported = Vec::new();
    let mut not_exported = Vec::new();
    let mut lines = output.lines();

    while let Some((x, is_exported)) = parse_package_object(&mut lines) {
        if is_exported {
            exported.push(x);
        } else {
            not_exported.push(x);
        }
    }

    let export_map = get_symbol_map(&exported)?;

    Ok(Package::new(package, exported, not_exported, export_map))
}

fn parse_package_object(lines: &mut Lines) -> Option<(Symbol, bool)> {
    let line = lines.next()?;

    let symbol_type = match line.chars().next()? {
        '0' => "object",
        '1' => "function",
        '2' => "lazydata",
        _ => panic!("Unknown symbol type"),
    };

    let exported = match line.chars().nth(1)? {
        '0' => false,
        '1' => true,
        _ => panic!("Unknown exported flag"),
    };

    let name = line[2..].to_string();

    if symbol_type == "function" {
        let mut signature = Vec::new();
        for line in lines.by_ref() {
            if line.is_empty() {
                break;
            }

            let mut parts = line.splitn(2, '=');
            let name = parts.next().unwrap().to_string();
            let default = match parts.next().unwrap() {
                "" => None,
                x => Some(x.to_string()),
            };
            signature.push(Arg { name, default });
        }

        Some((Symbol::Function { name, signature }, exported))
    } else {
        Some((Symbol::Object { name }, exported))
    }
}

pub fn installed_packages() -> Result<HashMap<String, PathBuf>> {
    let lib_paths = get_lib_paths()?;

    let mut packages = HashMap::new();

    for path in lib_paths {
        get_lib_packages(&mut packages, path)?;
    }

    Ok(packages)
}

fn get_lib_paths() -> Result<Vec<PathBuf>> {
    let command_result = Command::new("R")
        .args(["--slave", "-e", "cat(.libPaths())"])
        .output()?;

    if !command_result.status.success() {
        anyhow::bail!("Failed to get library paths")
    }

    let output = String::from_utf8(command_result.stdout)?;

    Ok(output.split(' ').map(PathBuf::from).collect())
}

fn get_lib_packages(packages: &mut HashMap<String, PathBuf>, path: PathBuf) -> Result<()> {
    for p in fs::read_dir(path)? {
        let potential_pkg = p?;
        if is_valid_package(&potential_pkg) {
            packages.insert(
                potential_pkg.file_name().to_string_lossy().to_string(),
                potential_pkg.path(),
            );
        }
    }

    Ok(())
}

fn is_valid_package(x: &DirEntry) -> bool {
    if !x.file_type().is_ok_and(|x| x.is_dir()) {
        return false;
    }

    let mut path = x.path();

    path.push("DESCRIPTION");

    path.exists()
}
