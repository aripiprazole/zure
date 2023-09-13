//! Virtual File System is a file system that emulates the real file
//! system with laziness and caching.
//!
//! Example commands:
//!
//! zure -I Batata:./batata.ml -I Abstract_tree:./abstract_tree.ml main.ml

use std::path::PathBuf;

use crate::db::LocalDb;
use crate::src::File;

pub trait ModuleLoader {
  fn root_folder(&self) -> PathBuf;

  /// Load a module from the file system.
  fn input(&self, path: String) -> eyre::Result<File>;
}

impl ModuleLoader for LocalDb {
  fn input(&self, path: String) -> eyre::Result<File> {
    let _ = path;
    todo!()
  }

  fn root_folder(&self) -> PathBuf {
    todo!()
  }
}
