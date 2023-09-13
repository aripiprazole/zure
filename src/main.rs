use std::path::PathBuf;

use clap::Parser;
use eyre::eyre;
use eyre::ContextCompat;
use salsa_2022::DebugWithDb;
use zure::parser::parse;
use zure::src::ModuleId;
use zure::vfs::ModuleLoader;

/// Zure's CLI. It's using the [`clap`] crate for parsing the commands,
/// and it's using [`miette`] for error handling.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
pub struct Cli {
  /// The input files.
  #[clap(short, long, required = false)]
  pub include: Option<Vec<String>>,

  /// The main module.
  pub main: String,
}

/// Zure's entrypoint.
fn main() -> eyre::Result<()> {
  env_logger::try_init()?;

  let cli = Cli::try_parse()?;
  let mut db = zure::db::LocalDb::default();
  let id = ModuleId::new(&db, cli.main.clone(), PathBuf::from(cli.main));

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
    .map(|(module_text, path_buf)| (module_text.clone(), ModuleId::new(&db, module_text, path_buf)))
    .collect::<im::HashMap<_, _>>();

  // Insert main module in the module tree so we can
  // access it later.
  db.modules.insert("Main".to_string(), id);

  // Parses the main module
  let module = parse(&db, db.input("Main".to_string())?);
  println!("{:#?}", module.debug_all(&db));

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
fn parse_include(include: String) -> eyre::Result<(String, PathBuf)> {
  let mut split = include.split(':');
  let module = split
    .next()
    .wrap_err_with(|| eyre!("Invalid include module: {}", include))?;
  let path = split
    .next()
    .wrap_err_with(|| eyre!("Invalid include path: {}", include))?;

  Ok((module.to_string(), path.into()))
}
