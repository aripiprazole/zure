use std::path::PathBuf;

/// Module file. It does tracks the imports, and the path of the file, for
/// incremental computing, and caching the module and build system.
///
/// It does contains the source code of the module, and the imports.
#[salsa::tracked]
pub struct Module {
  pub path: File,

  #[return_ref]
  pub imports: Vec<Import>,
}

/// The identifier of a module. It does contains the path of the module, and
/// the name of the module.
#[salsa::input]
pub struct ModuleId {
  #[return_ref]
  pub text: String,

  #[return_ref]
  pub path: PathBuf,
}

/// File is a source file. It does contains the source code of the file, and
/// the path of the file, for incremental computing, and caching the module and
/// build system.
#[salsa::input]
pub struct File {
  #[id]
  pub id: ModuleId,

  #[return_ref]
  pub contents: String,
}

/// Imports are references to another files. They are used to track the
/// dependencies of the module.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import {
  pub path: String,
}
