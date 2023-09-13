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
}

impl salsa::Database for LocalDb {}

impl salsa::ParallelDatabase for LocalDb {
  fn snapshot(&self) -> salsa::Snapshot<Self> {
    salsa::Snapshot::new(LocalDb {
      storage: self.storage.snapshot(),
    })
  }
}
