use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

use anyhow::Result;

use camino::Utf8PathBuf;
use flate2::bufread::GzDecoder;

fn read_package_info() {}

type Decoder = GzDecoder<BufReader<File>>;

fn main() {
    let path = env::args().nth(1).unwrap();

    read_rds(Utf8PathBuf::from(path)).unwrap();
}

fn read_rds(file: Utf8PathBuf) -> Result<()> {
    let mut decoder = GzDecoder::new(BufReader::new(File::open(file)?));

    let format = get_format(&mut decoder)?;

    println!("format: {:?}", format);

    Ok(())
}

#[derive(Debug)]
enum Format {
    Ascii,
    Binary,
    Xdr,
    Any,
}

fn get_format(decoder: &mut Decoder) -> Result<Format> {
    let mut buf = [0u8; 2];

    decoder.read_exact(&mut buf)?;

    Ok(match buf[0] as char {
        'A' => Format::Ascii,
        'B' => Format::Binary,
        'X' => Format::Xdr,
        '\n' => {
            if buf[1] as char == 'A' {
                decoder.read_exact(&mut [0u8])?;

                Format::Ascii
            } else {
                Format::Any
            }
        }
        _ => Format::Any,
    })
}

fn read_int(decoder: &mut Decoder, format: Format) -> Result<RInteger> {
    match format {
        Format::Ascii => Ok(parse_int(&read_word(decoder)?)?),
        Format::Binary => todo!(),
        Format::Xdr => todo!(),
        Format::Any => todo!(),
    }
}

fn parse_int(x: &str) -> Result<RInteger> {
    if x == "NA" {
        Ok(RInteger::NA)
    } else {
        Ok(RInteger::Int(x.parse()?))
    }
}

fn read_word(decoder: &mut Decoder) -> Result<String> {
    for b in decoder.bytes() {
        if !(b? as char).is_whitespace() {
            break;
        }
    }

    let mut result = String::new();
    for b in decoder.bytes() {
        let c = b? as char;
        if c.is_whitespace() {
            break;
        }

        result.push(c);
    }

    Ok(result)
}

enum RInteger {
    Int(usize),
    NA,
}
