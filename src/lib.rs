//! The main file of the project. It contains the library functions, and it's
//! used by the binary crate, which is the main entry point of the program.
//!
//! It contains the code that can be used as library, like lex, parser, typing, codegen,
//! evaluation.
//!
//! The configurations that are applied to CLI are done by the `main.rs` binary crate.

/// The local were the incremental components are found. It's used by the
/// `salsa` crate to incremental computing.
#[salsa::jar(db = ZureDb)]
pub struct Jar;

/// The database trait were we find the incremental components, and
/// functions to interact with the database.
/// 
/// It's implemented by the [`crate::db::LocalDb`] struct.
pub trait ZureDb: salsa::DbWithJar<Jar> {}

impl<Db: salsa::DbWithJar<Jar>> ZureDb for Db {}

/// Re-export `salsa` from `salsa-2022`
extern crate salsa_2022 as salsa;

// SECTION: Module re-exports
pub mod db;
