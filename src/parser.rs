use std::path::PathBuf;

use miette::NamedSource;
use miette::SourceSpan;

use self::lexing::Token;
use self::lexing::TokenKind;
use crate::src::File;
use crate::src::Module;
use crate::src::Span;

/// Parses file into a module. It does track the imports, and the path of the
/// contents, for incremental computing, and caching the module and build
/// system.
#[salsa::tracked]
pub fn parse(db: &dyn crate::ZureDb, file: File) -> Module {
  parsing::run_parser(db, file).unwrap()
}

#[salsa::accumulator]
pub struct Failure(InnerError);

/// Inner specified error for the parser. It's useful
/// to debug the parser.
///
/// If a function returns `Result<..., InnerError>` it should report an unrecoverable
/// error, it's a result because we can't recover, so it should stop the chain of the
/// parser.
#[derive(miette::Diagnostic, thiserror::Error, Debug, Clone)]
#[diagnostic(url(docsrs))]
pub enum InnerError {
  /// Publish the unexpected token error, it's like the parser expected `:=`
  /// but found `:==`, and it reports the error to the end-to-end user.
  ///
  /// It's useful to debug the parser and the source code if you are an end-to-end
  /// user.
  #[error("expected {expected}, found {found}")]
  #[diagnostic(code(E001P), help("replace the text with {expected}"))]
  UnexpectedToken {
    #[label("here")]
    at: SourceSpan,

    /// The token that was found.
    found: Token,

    /// The token that was expected.
    expected: TokenKind,
  },

  /// Publish the unexpected end of file error, it's like the parser expected
  /// `:=` but found `EOF`, and it reports the error to the end-to-end user.
  ///
  /// The file ended unexpectedly.
  #[error("unexpectedly found the end of file")]
  #[diagnostic(code(E002P), help("write more code to the file"))]
  Eof {
    #[label("here")]
    at: SourceSpan,
  },

  /// Fuel of the parser ended, so we can't continue parsing, it's just a
  /// debug error.
  ///
  /// It's annotated by `cfg(debug_assertions)` because it's only useful
  /// when we are debugging the parser.
  #[cfg(debug_assertions)]
  #[error("fuel exhausted")]
  #[diagnostic(code(E000P))]
  OutOfFuel {
    #[label("here")]
    at: SourceSpan,
  },
}

/// The error type for the parser. It's useful to debug the parser
/// or report errors to the final end-to-end user.
///
/// The parser can report multiple errors at once, so it's useful
/// to debug the parser.
#[derive(miette::Diagnostic, thiserror::Error, Debug)]
#[error("could not parse due the following errors")]
#[diagnostic(code(E000P))]
pub struct ParseError {
  /// Source code that is shown in the diagnostic to the end-to-end
  /// user.
  #[source_code]
  pub source_code: NamedSource,

  /// The related errors that are used to present the diagnostic
  /// to the end-to-end user.
  #[related]
  pub errors: Vec<InnerError>,
}

/// The part of the parser that handles lexing and tokenization.
mod lexing {
  use std::fmt::Display;

  use chumsky::prelude::*;

  pub type Failure<'a> = extra::Err<Rich<'a, char, SimpleSpan>>;
  pub type TokenSet = Vec<(Token, SimpleSpan)>;

  /// Kind of lexeme or token that will be used in the lexer
  /// to tokenize the input and parse it.
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum TokenKind {
    Number,
    String,
    Symbol,
    Open,
  }

  impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
        TokenKind::Number => write!(f, "<number>"),
        TokenKind::String => write!(f, "<string>"),
        TokenKind::Symbol => write!(f, "<symbol>"),
        TokenKind::Open => write!(f, "open"),
      }
    }
  }

  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct Token {
    pub data: TokenKind,
    pub text: String,
  }

  impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self.data {
        TokenKind::Number => write!(f, "{}", self.text),
        TokenKind::String => write!(f, "\"{}\"", self.text),
        TokenKind::Symbol => write!(f, "{}", self.text),
        TokenKind::Open => write!(f, "open"),
      }
    }
  }

  /// Create a new token with the given data and text.
  fn new_token(data: TokenKind, text: &str) -> Token {
    Token {
      data,
      text: text.to_string(),
    }
  }

  /// Identification lexer parser combinator
  fn ident_lexer<'a>() -> impl Parser<'a, &'a str, Token, Failure<'a>> {
    use TokenKind::*;
    text::ident().map(|text| match text {
      "open" => new_token(Open, text),
      _ => new_token(Symbol, text),
    })
  }

  /// Lexer parser combinator
  fn lexer<'a>() -> impl Parser<'a, &'a str, TokenSet, Failure<'a>> {
    use TokenKind::*;

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
  use std::cell::Cell;

  use chumsky::prelude::*;

  use super::lexing::Token;
  use super::lexing::TokenKind;
  use super::lexing::TokenKind::*;
  use super::*;
  use crate::error::failwith;
  use crate::src::Span;
  use crate::ZureDb;

  const MAX_FUEL: usize = 256;

  pub struct Parser<'src> {
    src: &'src str,
    tokens: &'src [(Token, SimpleSpan)],
    index: usize,

    #[cfg(debug_assertions)]
    fuel: Cell<usize>,
  }

  /// Report an span error to the end-to-end user.
  fn fix_span(span: SourceSpan) -> Span {
    todo!()
  }

  /// Creates a new zure local span based on the [`chumsky`] span.
  fn create_zure_span(span: SimpleSpan) -> SourceSpan {
    todo!()
  }

  impl<'src> Parser<'src> {
    #[inline]
    fn peek(&mut self) -> Result<(&Token, SourceSpan), InnerError> {
      self.lookahead(0)
    }

    /// Lookahead the next token. It does return a result
    /// that will be an [`Err`] if the file ends unexpectedly.
    fn lookahead(&self, nth: usize) -> Result<(&Token, SourceSpan), InnerError> {
      // Reports to the user when the file ends unexpectedly. Like if
      // the user forgot to close a parenthesis.
      if self.index >= self.tokens.len() {
        return Err(InnerError::Eof {
          at: create_zure_span(SimpleSpan::new(self.src.len(), self.src.len())),
        });
      }

      self.consume_fuel()?;

      let (tok, span) = &self.tokens[self.index + nth];
      Ok((tok, create_zure_span(*span)))
    }

    /// Bump the parser to the next token.
    fn advance(&mut self) -> Result<(), InnerError> {
      // Reports to the user when the file ends unexpectedly. Like if
      // the user forgot to close a parenthesis.
      if self.index >= self.tokens.len() {
        return Err(InnerError::Eof {
          at: create_zure_span(SimpleSpan::new(self.src.len(), self.src.len())),
        });
      }

      self.index += 1;
      self.refuel()?;

      Ok(())
    }

    /// Gets more fuel to the parser in production mode
    #[cfg(not(debug_assertions))]
    #[inline(always)]
    fn refuel(&self) -> Result<(), InnerError> {
      Ok(())
    }

    /// Gets more fuel to the parser
    #[cfg(debug_assertions)]
    fn refuel(&self) -> Result<(), InnerError> {
      self.fuel.set(MAX_FUEL);

      Ok(())
    }

    /// Consumes fuel in production code, or just return
    /// if we are in debug mode.
    #[cfg(not(debug_assertions))]
    #[inline(always)]
    fn consume_fuel(&self) -> Result<(), InnerError> {
      Ok(())
    }

    /// Consumes fuel in debug code, returning an error if
    /// the fuel is exhausted.
    #[cfg(debug_assertions)]
    fn consume_fuel(&self) -> Result<(), InnerError> {
      #[cfg(debug_assertions)]
      if self.fuel.get() == 0 {
        return Err(InnerError::OutOfFuel {
          at: create_zure_span(SimpleSpan::new(self.src.len(), self.src.len())),
        });
      }

      self.fuel.set(self.fuel.get() - 1);

      Ok(())
    }
  }

  fn expect(db: &dyn ZureDb, p: &mut Parser, expected: TokenKind) -> Result<(), InnerError> {
    let (token, at) = p.peek()?;
    if token.data == expected {
      p.advance()?;
    } else {
      failwith(db, InnerError::UnexpectedToken {
        at,
        found: token.clone(),
        expected,
      });
    }

    Ok(())
  }

  /// Run parser on the given input. If an error occurs, return the error with
  /// miette.
  pub fn run_parser(db: &dyn crate::ZureDb, file: File) -> miette::Result<Module> {
    let contents = file.contents(db);
    let tokens = lexing::run_lexer(file.contents(db)).unwrap();
    todo!()
    // Ok(Module::new(db, file, imports, vec![]))
  }
}
