use crate::src::{File, Module};

#[salsa::tracked]
pub fn parse(db: &dyn crate::ZureDb, file: File) -> Module {
  let _ = db;
  let _ = file;
  todo!()
}