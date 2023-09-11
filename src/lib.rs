//! The main file of the project. It contains the library functions, and it's
//! used by the binary crate, which is the main entry point of the program.
//!
//! It contains the code that can be used as library, like lex, parser, typing, codegen,
//! evaluation.
//!
//! The configurations that are applied to CLI are done by the `main.rs` binary crate.

pub mod ast;
pub mod constr;
pub mod cst;
pub mod hir;
pub mod nfe;
pub mod span;
pub mod tok;
pub mod typer;
pub mod unifier;
pub mod val;
