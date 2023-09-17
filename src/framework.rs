use test::TestDesc;
use test::TestDescAndFn;
use test::TestName;

extern crate test;

pub struct TestSuite {
  pub directory: &'static str,
  pub run: fn(&str) -> Result<(), miette::Report>,
}

pub fn zure_test_suite(tests: &[&TestSuite]) {}

#[macro_export]
macro_rules! make_test {
  ($directory:expr, $run:expr) => {
    #[test_case]
    const TEST: $crate::framework::TestSuite = $crate::framework::TestSuite {
      directory: $directory,
      run: $run,
    };
  };
}

pub use make_test;