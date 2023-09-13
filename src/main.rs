use std::path::PathBuf;

use clap::Parser;
use eyre::eyre;
use eyre::ContextCompat;
use zure::src::ModuleId;

/// Zure's CLI. It's using the [`clap`] crate for parsing the commands,
/// and it's using [`miette`] for error handling.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
pub struct Cli {
  /// The input files.
  #[clap(short, long, required = false)]
  pub include: Option<Vec<String>>,
}

/// Zure's entrypoint.
fn main() -> eyre::Result<()> {
  env_logger::try_init()?;

  let cli = Cli::try_parse()?;
  let mut db = zure::db::LocalDb::default();

  // This parses the input commands and loads the modules, it's
  // used to map the modules to the files in the real file system,
  // so we can read the contents of the files.
  db.modules = cli
    .include
    .unwrap_or_default()
    .into_iter()
    .map(parse_include)
    .collect::<eyre::Result<Vec<_>>>()?
    .into_iter()
    .map(|(path, module)| (path.clone(), ModuleId::new(&db, module, path)))
    .collect::<im::HashMap<_, _>>();

  // TODO: validate integrity of files
  //       if it's a file or folder
  //       if it's a valid module name

  Ok(())
}


/// Parses include instruction of include in the CLI, the format should be like
/// 
/// ```bash
/// zure -I <module>:<path>
/// ```
fn parse_include(include: String) -> eyre::Result<(PathBuf, String)> {
  let mut split = include.split(':');
  let module = split
    .next()
    .wrap_err_with(|| eyre!("Invalid include module: {}", include))?;
  let path = split
    .next()
    .wrap_err_with(|| eyre!("Invalid include path: {}", include))?;

  Ok((path.into(), module.to_string()))
}
