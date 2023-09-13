use miette::IntoDiagnostic;

/// Zure's entrypoint.
fn main() -> miette::Result<()> {
  env_logger::try_init().into_diagnostic()?;
  Ok(())
}
