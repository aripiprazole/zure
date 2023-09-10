//! Tokenization is a pretty important step in the compilation process, it's the step that
//! transform the source code into a stream of tokens, that can be used to parse the source
//! code into an CST, and further, into the AST.
//!
//! The tokenization process is pretty simple, it's just a simple state machine that
//! does transform the source code into a stream of tokens. Here we use [`chumsky`].

use std::fmt::Display;

pub use Ctrl::*;
pub use Direction::*;
pub use EqlMode::*;
pub use Token::*;

pub use self::BinOp::*;
pub use self::Delim::*;
pub use self::Sep::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Direction {
  Left,
  Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ctrl {
  Open,
  Close,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EqlMode {
  Cmp,
  Type,
  Assign,
}

/// The unit of lexing process that contains more semantic information, like location, token kind [`Token`],
/// and the text that was parsed.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Lexeme<'src> {
  pub text: &'src str,
  pub token: Token<'src>,
  pub span: crate::span::SrcPos,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
  Or,
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  Neq,
  Ltl,
  Lte,
  Gtl,
  Gte,
  And,
  Eql(EqlMode),
  Arrow(Direction),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Sep {
  Comma,
  Semi,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Delim {
  Parens,
  Brackets,
  Braces,
}

/// The base unit of the tokenization process, that is called lexer in the
/// zure language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token<'src> {
  // SECTION: Keywords
  Fun,
  Let,
  If,
  Else,
  While,
  Match,
  With,
  Inductive,
  Coinductive,
  Record,
  Return,

  // SECTION: Symbols
  Delim(Delim, Ctrl),
  Sep(Sep),

  // SECTION: Operators
  BinOp(BinOp),
  At,
  Hash,
  Bang,

  // SECTION: Literals
  Int(&'src str),
  Str(&'src str),

  // SECTION: Identifiers
  Ident(&'src str),
}

impl Display for Token<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Fun => write!(f, "fun"),
      Let => write!(f, "let"),
      If => write!(f, "if"),
      Else => write!(f, "else"),
      While => write!(f, "while"),
      Match => write!(f, "match"),
      With => write!(f, "with"),
      Inductive => write!(f, "inductive"),
      Coinductive => write!(f, "conductive"),
      Record => write!(f, "record"),
      Return => write!(f, "return"),
      At => write!(f, "@"),
      Hash => write!(f, "#"),
      Bang => write!(f, "!"),
      Int(v) => write!(f, "{v}"),
      Str(v) => write!(f, "\"{v}\""),
      Ident(v) => write!(f, "{v}"),
      Sep(Comma) => write!(f, ","),
      Sep(Semi) => write!(f, ";"),
      Delim(Parens, Open) => write!(f, "("),
      Delim(Parens, Close) => write!(f, ")"),
      Delim(Braces, Open) => write!(f, "{{"),
      Delim(Braces, Close) => write!(f, "}}"),
      Delim(Brackets, Open) => write!(f, "[["),
      Delim(Brackets, Close) => write!(f, "]]"),
      BinOp(Add) => write!(f, "+"),
      BinOp(Sub) => write!(f, "-"),
      BinOp(Mul) => write!(f, "*"),
      BinOp(Div) => write!(f, "/"),
      BinOp(Mod) => write!(f, "%"),
      BinOp(And) => write!(f, "&&"),
      BinOp(Or) => write!(f, "||"),
      BinOp(Neq) => write!(f, "!="),
      BinOp(Ltl) => write!(f, "<"),
      BinOp(Gtl) => write!(f, ">"),
      BinOp(Lte) => write!(f, "<="),
      BinOp(Gte) => write!(f, ">="),
      BinOp(Eql(Assign)) => write!(f, "="),
      BinOp(Eql(Cmp)) => write!(f, "=="),
      BinOp(Eql(Type)) => write!(f, ":="),
      BinOp(Arrow(Left)) => write!(f, "<-"),
      BinOp(Arrow(Right)) => write!(f, "->"),
    }
  }
}
