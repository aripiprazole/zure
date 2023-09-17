use crate::src::ModuleId;

/// This is the local implementation of the database. It does implement the cache
/// and the incremental computing stuff.
///
/// It carries the LRU-cache, the graphs, etc.
#[derive(Default)]
#[salsa::db(crate::Jar)]
pub struct LocalDb {
  /// The local were salsa stores the caches, and the already-computed
  /// stuff.
  pub storage: salsa::Storage<LocalDb>,
  pub dependency_graph: crate::vfs::DependencyGraph,
  pub modules: im::HashMap<String, ModuleId>,
}

impl salsa::Database for LocalDb {}

impl salsa::ParallelDatabase for LocalDb {
  fn snapshot(&self) -> salsa::Snapshot<Self> {
    salsa::Snapshot::new(LocalDb {
      storage: self.storage.snapshot(),
      modules: self.modules.clone(),
      dependency_graph: self.dependency_graph.clone(),
    })
  }
}
