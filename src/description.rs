use std::{
    collections::HashMap,
    fs,
    iter::{Enumerate, Peekable},
    path::PathBuf,
    str::Chars,
};

use anyhow::Result;
use camino::Utf8PathBuf;

use crate::grammar::FileSpan;

pub fn find_description(path: Utf8PathBuf) -> Result<Option<DescriptionFile>> {
    path.read_dir()?
        .filter_map(|x| x.ok())
        .find(|x| x.file_name() == "DESCRIPTION")
        .map(|x| read_description(x.path()))
        .transpose()
}

pub fn read_description(file: PathBuf) -> Result<DescriptionFile> {
    let text = fs::read_to_string(&file)?;

    let lines = text.lines();

    let mut description = Description::new();

    let mut errors = Vec::new();

    let mut field: Option<String> = None;
    let mut field_span: Option<FileSpan> = None;
    let mut value = String::new();
    for (line_number, line) in lines.enumerate() {
        let mut chars = line.chars().enumerate().peekable();

        if chars.peek().is_some_and(|(_, x)| x.is_whitespace()) {
            if field.is_some() {
                consume_whitespace(&mut chars);

                if !value.is_empty() {
                    value.push('\n');
                }

                let content: String = chars.map(|(_, x)| x).collect();

                if content != "." {
                    value.push_str(&content);
                }

                continue;
            } else {
                errors.push(DescriptionError {
                    error_type: DescriptionErrorType::ExpectedField,
                    span: Some(FileSpan::single(line_number, 0)),
                });
                consume_whitespace(&mut chars);
            }
        }

        if let Some(field) = field {
            if let Err(t) = description.add(field, value) {
                errors.push(DescriptionError {
                    error_type: t,
                    span: Some(field_span.unwrap()),
                })
            }
            value = String::new();
        }
        field = None;
        field_span = None;

        let mut string_field = String::new();
        let mut seen_colon = false;

        let mut end = 0;
        for (i, c) in chars.by_ref() {
            end = i;
            if c == ':' {
                seen_colon = true;
                break;
            }

            string_field.push(c);
        }

        if !seen_colon {
            errors.push(DescriptionError {
                error_type: DescriptionErrorType::ExpectedColon,
                span: Some(FileSpan::new(line_number, end, line_number, end)),
            });
            continue;
        }

        field = Some(string_field);
        field_span = Some(FileSpan::new(line_number, 0, line_number, end));

        consume_whitespace(&mut chars);

        value.push_str(&chars.map(|(_, x)| x).collect::<String>());
    }

    errors.extend(description.validate());

    Ok(DescriptionFile {
        description,
        file,
        errors,
    })
}

fn consume_whitespace(chars: &mut Peekable<Enumerate<Chars>>) {
    while chars.next_if(|(_, x)| x.is_whitespace()).is_some() {}
}

pub struct DescriptionFile {
    pub description: Description,
    pub file: PathBuf,
    pub errors: Vec<DescriptionError>,
}

// https://cran.r-project.org/doc/manuals/R-exts.html#The-DESCRIPTION-file
pub struct Description {
    // Mandatory
    pub package: Option<String>,
    pub version: Option<String>,
    pub license: Option<String>,
    pub description: Option<String>,
    pub title: Option<String>,

    // Either Authors@R or Author, Maintainer must exist
    pub authors_at_r: Option<String>,
    pub author: Option<String>,
    pub maintainer: Option<String>,

    // Optional
    pub copyright: Option<String>,
    pub date: Option<String>,
    pub depends: Option<Vec<String>>,
    pub imports: Option<Vec<String>>,
    pub suggests: Option<Vec<String>>,
    pub enhances: Option<Vec<String>>,
    pub linking_to: Option<String>,
    pub additional_repositories: Option<String>,
    pub url: Option<String>,
    pub bug_reports: Option<String>,
    pub contact: Option<String>,
    pub priority: Option<String>,
    pub collate: Option<String>,
    pub lazy_data: Option<String>,
    pub keep_source: Option<String>,
    pub byte_compile: Option<String>,
    pub use_lto: Option<String>,
    pub staged_install: Option<String>,
    pub build_vignettes: Option<String>,
    pub vignette_builder: Option<String>,
    pub encoding: Option<String>,
    pub needs_compilation: Option<String>,
    pub os_type: Option<String>,
    pub type_: Option<String>,
    pub classification_acm: Option<String>,
    pub classification_msc: Option<String>,
    pub language: Option<String>,
    pub rd_macros: Option<String>,

    // Not relevant / No longer used
    pub lazy_load: Option<String>,
    pub zip_data: Option<String>,
    pub biarch: Option<String>,

    pub config: HashMap<String, String>,
}

impl Description {
    pub fn new() -> Self {
        Self {
            package: None,
            version: None,
            license: None,
            description: None,
            title: None,
            authors_at_r: None,
            author: None,
            maintainer: None,
            copyright: None,
            date: None,
            depends: None,
            imports: None,
            suggests: None,
            enhances: None,
            linking_to: None,
            additional_repositories: None,
            url: None,
            bug_reports: None,
            contact: None,
            priority: None,
            collate: None,
            lazy_data: None,
            keep_source: None,
            byte_compile: None,
            use_lto: None,
            staged_install: None,
            build_vignettes: None,
            vignette_builder: None,
            encoding: None,
            needs_compilation: None,
            os_type: None,
            type_: None,
            classification_acm: None,
            classification_msc: None,
            language: None,
            rd_macros: None,
            lazy_load: None,
            zip_data: None,
            biarch: None,
            config: HashMap::new(),
        }
    }

    pub fn field_is_none(&self, field: &str) -> bool {
        match field {
            "Package" => self.package.is_none(),
            "Version" => self.version.is_none(),
            "License" => self.license.is_none(),
            "Description" => self.description.is_none(),
            "Title" => self.title.is_none(),
            "Authors@R" => self.authors_at_r.is_none(),
            "Author" => self.author.is_none(),
            "Maintainer" => self.maintainer.is_none(),
            "Copyright" => self.copyright.is_none(),
            "Date" => self.date.is_none(),
            "Depends" => self.depends.is_none(),
            "Imports" => self.imports.is_none(),
            "Suggests" => self.suggests.is_none(),
            "Enhances" => self.enhances.is_none(),
            "LinkingTo" => self.linking_to.is_none(),
            "AdditionalRepositories" => self.additional_repositories.is_none(),
            "URL" => self.url.is_none(),
            "BugReports" => self.bug_reports.is_none(),
            "Contact" => self.contact.is_none(),
            "Priority" => self.priority.is_none(),
            "Collate" => self.collate.is_none(),
            "LazyData" => self.lazy_data.is_none(),
            "KeepSource" => self.keep_source.is_none(),
            "ByteCompile" => self.byte_compile.is_none(),
            "UseLto" => self.use_lto.is_none(),
            "StagedInstall" => self.staged_install.is_none(),
            "BuildVignettes" => self.build_vignettes.is_none(),
            "VignetteBuilder" => self.vignette_builder.is_none(),
            "Encoding" => self.encoding.is_none(),
            "NeedsCompilation" => self.needs_compilation.is_none(),
            "OS_type" => self.os_type.is_none(),
            "Type" => self.type_.is_none(),
            "Classification/ACM" => self.classification_acm.is_none(),
            "Classification/ACM-2012" => self.classification_acm.is_none(),
            "Classification/MSC" => self.classification_msc.is_none(),
            "Classification/MSC-2010" => self.classification_msc.is_none(),
            "Language" => self.language.is_none(),
            "RdMacros" => self.rd_macros.is_none(),
            "LazyLoad" => self.lazy_load.is_none(),
            "ZipData" => self.zip_data.is_none(),
            "Biarch" => self.biarch.is_none(),
            x => {
                if !x.starts_with("Config/") {
                    return true;
                }

                !self.config.contains_key(field)
            }
        }
    }

    pub fn add(&mut self, field: String, value: String) -> Result<(), DescriptionErrorType> {
        if !self.field_is_none(&field) {
            return Err(DescriptionErrorType::DuplicateField(field));
        }

        match field.as_str() {
            "Package" => self.package = Some(value),
            "Version" => self.version = Some(value),
            "License" => self.license = Some(value),
            "Description" => self.description = Some(value),
            "Title" => self.title = Some(value),
            "Authors@R" => self.authors_at_r = Some(value),
            "Author" => self.author = Some(value),
            "Maintainer" => self.maintainer = Some(value),
            "Copyright" => self.copyright = Some(value),
            "Date" => self.date = Some(value),
            "Depends" => self.depends = Some(parse_packages(value)),
            "Imports" => self.imports = Some(parse_packages(value)),
            "Suggests" => self.suggests = Some(parse_packages(value)),
            "Enhances" => self.enhances = Some(parse_packages(value)),
            "LinkingTo" => self.linking_to = Some(value),
            "AdditionalRepositories" => self.additional_repositories = Some(value),
            "URL" => self.url = Some(value),
            "BugReports" => self.bug_reports = Some(value),
            "Contact" => self.contact = Some(value),
            "Priority" => self.priority = Some(value),
            "Collate" => self.collate = Some(value),
            "LazyData" => self.lazy_data = Some(value),
            "KeepSource" => self.keep_source = Some(value),
            "ByteCompile" => self.byte_compile = Some(value),
            "UseLto" => self.use_lto = Some(value),
            "StagedInstall" => self.staged_install = Some(value),
            "BuildVignettes" => self.build_vignettes = Some(value),
            "VignetteBuilder" => self.vignette_builder = Some(value),
            "Encoding" => self.encoding = Some(value),
            "NeedsCompilation" => self.needs_compilation = Some(value),
            "OS_type" => self.os_type = Some(value),
            "Type" => self.type_ = Some(value),
            "Classification/ACM" => self.classification_acm = Some(value),
            "Classification/ACM-2012" => self.classification_acm = Some(value),
            "Classification/MSC" => self.classification_msc = Some(value),
            "Classification/MSC-2010" => self.classification_msc = Some(value),
            "Language" => self.language = Some(value),
            "RdMacros" => self.rd_macros = Some(value),
            "LazyLoad" => self.lazy_load = Some(value),
            "ZipData" => self.zip_data = Some(value),
            "Biarch" => self.biarch = Some(value),
            x => {
                if x.starts_with("Config/") {
                    self.config.insert(field, value);
                } else {
                    return Err(DescriptionErrorType::UnknownField);
                }
            }
        }

        Ok(())
    }

    pub fn validate(&self) -> Vec<DescriptionError> {
        let mut errors = Vec::new();

        if self.package.is_none() {
            errors.push(DescriptionError {
                error_type: DescriptionErrorType::MissingField("Package".to_string()),
                span: None,
            });
        }

        if self.version.is_none() {
            errors.push(DescriptionError {
                error_type: DescriptionErrorType::MissingField("Version".to_string()),
                span: None,
            });
        }

        if self.license.is_none() {
            errors.push(DescriptionError {
                error_type: DescriptionErrorType::MissingField("License".to_string()),
                span: None,
            });
        }

        if self.description.is_none() {
            errors.push(DescriptionError {
                error_type: DescriptionErrorType::MissingField("Description".to_string()),
                span: None,
            });
        }

        if self.title.is_none() {
            errors.push(DescriptionError {
                error_type: DescriptionErrorType::MissingField("Title".to_string()),
                span: None,
            });
        }

        if self.authors_at_r.is_none() {
            if self.author.is_none() {
                if self.maintainer.is_none() {
                    errors.push(DescriptionError {
                        error_type: DescriptionErrorType::MissingField("Authors@R".to_string()),
                        span: None,
                    });
                } else {
                    errors.push(DescriptionError {
                        error_type: DescriptionErrorType::MissingField("Author".to_string()),
                        span: None,
                    });
                }
            } else if self.maintainer.is_none() {
                errors.push(DescriptionError {
                    error_type: DescriptionErrorType::MissingField("Maintainer".to_string()),
                    span: None,
                });
            }
        }

        errors
    }

    pub fn depends(&self) -> Option<&Vec<String>> {
        self.depends.as_ref()
    }

    pub fn imports(&self) -> Option<&Vec<String>> {
        self.imports.as_ref()
    }

    pub fn suggests(&self) -> Option<&Vec<String>> {
        self.suggests.as_ref()
    }

    pub fn enhances(&self) -> Option<&Vec<String>> {
        self.enhances.as_ref()
    }
}

fn parse_packages(value: String) -> Vec<String> {
    value
        .split(',')
        .map(|x| x.trim().to_string())
        .filter(|x| !x.is_empty())
        .collect()
}

pub struct DescriptionError {
    error_type: DescriptionErrorType,
    span: Option<FileSpan>,
}

pub enum DescriptionErrorType {
    DuplicateField(String),
    UnknownField,
    ExpectedColon,
    ExpectedField,
    MissingField(String),
}
