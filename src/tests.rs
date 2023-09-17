use zure::framework::make_test;

make_test!("/test_suiter/typer/leibnz", |file| {
  use miette::NamedSource;
  use zure::parser::Failure;
  use zure::parser::ParseError;
  use zure::src::new_file;

  let text = std::fs::read_to_string(&file)?;
  let db = zure::db::LocalDb::default();
  let file = new_file(&db, file, text.clone());

  let _ = zure::parser::parse(&db, file);
  let failures = zure::parser::parse::accumulated::<Failure>(&db, file);

  if failures.is_empty() {
    Ok(())
  } else {
    Err(ParseError {
      source_code: NamedSource::new("leibnz", text),
      errors: failures,
    })?
  }
});
