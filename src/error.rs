use std::sync::Arc;

use crate::ZureDb;

/// The error accumulator that will be presented to the user using
/// [`bupropion`] crate.
///
/// It's used to accumulate errors from the parser, typer, etc.
#[salsa::accumulator]
pub struct Failure(Arc<miette::Report>);

/// Publishes an error to the database.
pub fn failwith<I: miette::Diagnostic + Sync + Send + 'static>(db: &dyn ZureDb, error: I) -> Arc<miette::Report> {
  let report = Arc::new(miette::Report::new(error));
  Failure::push(db, report.clone());
  report
}
