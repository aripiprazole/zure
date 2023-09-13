use std::path::PathBuf;

use crate::src::{Module, ModuleId};

/// This is the local implementation of the database. It does implement the cache
/// and the incremental computing stuff.
///
/// It carries the LRU-cache, the graphs, etc.
#[derive(Default)]
#[salsa::db(crate::Jar)]
pub struct LocalDb {
  /// The local were salsa stores the caches, and the already-computed
  /// stuff.
  storage: salsa::Storage<LocalDb>,

  files: dashmap::DashMap<PathBuf, Module>,

  pub modules: im::HashMap<PathBuf, ModuleId>,
}

impl salsa::Database for LocalDb {}

impl salsa::ParallelDatabase for LocalDb {
  fn snapshot(&self) -> salsa::Snapshot<Self> {
    salsa::Snapshot::new(LocalDb {
      storage: self.storage.snapshot(),
      modules: self.modules.clone(),
      files: self.files.clone(),
    })
  }
}
