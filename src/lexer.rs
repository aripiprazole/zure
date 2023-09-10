use std::fmt::Display;

pub use Ctrl::*;
pub use Direction::*;
pub use Token::*;

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

/// The unit of lexing process that contains more semantic information, like location, token kind [`Token`],
/// and the text that was parsed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lexeme<'src> {
  pub text: &'src str,
  pub token: Token<'src>,
  pub span: crate::span::SrcPos,
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
  Paren(Ctrl),
  Brace(Ctrl),
  Bracket(Ctrl),
  Comma,
  Colon,
  Semi,

  // SECTION: Operators
  Plus,
  Minus,
  Star,
  Slash,
  Percent,
  Ampersand,
  Pipe,
  Hash,
  At,
  Bang,
  Eql,
  Arrow(Direction),

  // SECTION: Comparisons
  Eq,
  Neq,
  Lt,
  Gt,
  Lte,
  Gte,

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
      Paren(Open) => write!(f, "("),
      Paren(Close) => write!(f, ")"),
      Brace(Open) => write!(f, "{{"),
      Brace(Close) => write!(f, "}}"),
      Bracket(Open) => write!(f, "[["),
      Bracket(Close) => write!(f, "]]"),
      Comma => write!(f, ","),
      Colon => write!(f, ":"),
      Semi => write!(f, ";"),
      Plus => write!(f, "+"),
      Minus => write!(f, "-"),
      Star => write!(f, "*"),
      Slash => write!(f, "/"),
      Percent => write!(f, "%"),
      Ampersand => write!(f, "&"),
      Pipe => write!(f, "|"),
      Hash => write!(f, "#"),
      At => write!(f, "@"),
      Bang => write!(f, "!"),
      Eql => write!(f, ":="),
      Arrow(Left) => write!(f, "<-"),
      Arrow(Right) => write!(f, "->"),
      Eq => write!(f, "="),
      Neq => write!(f, "!="),
      Lt => write!(f, "<"),
      Gt => write!(f, ">"),
      Lte => write!(f, "<="),
      Gte => write!(f, ">="),
      Int(v) => write!(f, "{v}"),
      Str(v) => write!(f, "\"{v}\""),
      Ident(v) => write!(f, "{v}"),
    }
  }
}
