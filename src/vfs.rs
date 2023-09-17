//! Virtual File System is a file system that emulates the real file
//! system with laziness and caching.
//!
//! Example commands:
//!
//! zure -I Batata:./batata.ml -I Abstract_tree:./abstract_tree.ml main.ml

use std::path::PathBuf;

use dashmap::mapref::entry::Entry;
use eyre::Context;

use crate::db::LocalDb;
use crate::src::File;

#[derive(Default, Clone)]
pub struct DependencyGraph {
  pub value: dashmap::DashMap<PathBuf, File>
}

pub trait ModuleLoader {
  fn root_folder(&self) -> PathBuf;

  /// Load a module from the file system.
  fn input(&self, path: String) -> eyre::Result<File>;
}

impl ModuleLoader for LocalDb {
  fn input(&self, path: String) -> eyre::Result<File> {
    // Gets the module id from the included files
    // associated with the path.
    let module_id = *self.modules.get(&path).unwrap_or_else(|| todo!("module not found"));

    let path = module_id
      .path(self)
      .canonicalize()
      .wrap_err_with(|| format!("failed to read {}", module_id.text(self)))?;

    // Tries to read the file from the cache. If can't read it, then
    // reads it from the file system.
    Ok(match self.dependency_graph.value.entry(path.clone()) {
      // If the file already exists in our cache then just return it.
      Entry::Occupied(entry) => *entry.get(),

      // If we haven't read this file yet set up the watch, read the
      // contents, store it in the cache, and return it.
      Entry::Vacant(entry) => {
        // Set up the watch before reading the contents to try to avoid
        // race conditions.
        //
        // ```
        // let watcher = &mut *self.file_watcher.lock().unwrap();
        // watcher
        //   .watcher()
        //   .watch(&path, RecursiveMode::NonRecursive)
        //   .unwrap();
        // ```

        let contents = std::fs::read_to_string(&path).wrap_err_with(|| format!("failed to read {}", path.display()))?;

        *entry.insert(File::new(self, module_id, contents))
      }
    })
  }

  fn root_folder(&self) -> PathBuf {
    todo!()
  }
}
