use crate::src::File;
use crate::src::Module;

/// Parses file into a module. It does track the imports, and the path of the
/// contents, for incremental computing, and caching the module and build
/// system.
#[salsa::tracked]
pub fn parse(db: &dyn crate::ZureDb, file: File) -> Module {
  parsing::run_parser(db, file).unwrap()
}

/// The part of the parser that handles lexing and tokenization.
mod lexing {
  use chumsky::prelude::*;

  pub type Failure<'a> = extra::Err<Rich<'a, char, SimpleSpan>>;
  pub type TokenSet = Vec<(Token, SimpleSpan)>;

  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum TokenData {
    Number,
    String,
    Symbol,
    Open,
  }

  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct Token {
    pub data: TokenData,
    pub text: String,
  }

  /// Create a new token with the given data and text.
  fn new_token(data: TokenData, text: &str) -> Token {
    Token {
      data,
      text: text.to_string(),
    }
  }

  /// Identification lexer parser combinator
  fn ident_lexer<'a>() -> impl Parser<'a, &'a str, Token, Failure<'a>> {
    use TokenData::*;
    text::ident().map(|text| match text {
      "open" => new_token(Open, text),
      _ => new_token(Symbol, text),
    })
  }

  /// Lexer parser combinator
  fn lexer<'a>() -> impl Parser<'a, &'a str, TokenSet, Failure<'a>> {
    use TokenData::*;

    let num = text::int(10)
      .then(just('.').then(text::digits(10)).or_not())
      .slice()
      .map(|text: &str| new_token(Number, text))
      .labelled("number");

    let string = just('"')
      .ignore_then(none_of('"').repeated())
      .then_ignore(just('"'))
      .map_slice(|text: &str| new_token(String, text))
      .labelled("string literal");

    let comment = just("//")
      .then(any().and_is(just('\n').not()).repeated())
      .padded()
      .labelled("comment");

    let token = num.or(string).or(ident_lexer());

    // If we encounter an error, skip and attempt to lex the next character as a token instead
    token
      .map_with_span(|tok, span| (tok, span))
      .padded_by(comment.repeated())
      .padded()
      .recover_with(skip_then_retry_until(any().ignored(), end()))
      .repeated()
      .collect()
  }

  /// Run token lexer on the given input. If an error occurs, return the error
  /// with miette.
  pub fn run_lexer(input: &str) -> miette::Result<TokenSet> {
    let tokens = lexer().parse(input).unwrap();
    Ok(tokens)
  }
}

/// Parsing module that will parse the tokens into an AST.
mod parsing {
  use chumsky::prelude::*;

  use super::lexing::Token;
  use super::lexing::TokenData::*;
  use super::*;
  use crate::src::Import;

  fn parser<'src>() -> impl Parser<
    'src,
    chumsky::input::SpannedInput<Token, SimpleSpan, &'src [(Token, SimpleSpan)]>,
    Vec<Import>,
    extra::Err<Rich<'src, lexing::Token, SimpleSpan>>,
  > {
    let import = select! { Token { data: Open, .. } => () }
      .ignored()
      .then(select! {
        Token { data: Symbol, text } => text
      })
      .map(|((), text)| Import {
        path: text.clone(),
        span: None,
      })
      .labelled("open declaration");

    import.repeated().collect::<Vec<_>>()
  }

  /// Run parser on the given input. If an error occurs, return the error with
  /// miette.
  pub fn run_parser(db: &dyn crate::ZureDb, file: File) -> miette::Result<Module> {
    let contents = file.contents(db);
    let tokens = lexing::run_lexer(file.contents(db)).unwrap();
    let tokens = tokens.as_slice().spanned((contents.len()..contents.len()).into());
    let imports = parser().parse(tokens).unwrap();
    Ok(Module::new(db, file, imports))
  }
}
