package tokyo.meg.script.lexer

import scala.collection.mutable._

import TokenCategory._

object DefaultReservedTokens:
  val MACRO = "MACRO"
  val COMMENT = "COMMENT"
  val SEMI = "SEMI"
  val COLON = "COLON"
  val WHERE = "WHERE"
  val ELSE = "ELSE"
  val COLONCOLON = "COLONCOLON"
  val EQUALS = "EQUALS"
  val GTLT = "GTLT"
  val LFUNNEL = "LFUNNEL"
  val RFUNNEL = "RFUNNEL"
  val QUEST = "QUEST"
  val IF = "IF"
  val PLACEHOLDER = "PLACEHOLDER"
  val AT = "AT"
  val QUESTCOLON = "QUESTCOLON"
  val ATCOLON = "ATCOLON"
  val LARROW = "LARROW"
  val PERIODPERIOD = "PERIODPERIOD"
  val PERIOD = "PERIOD"
  val RARROW = "RARROW"
  val DOLLAR = "DOLLAR"
  val DEFAULT_WHERE = ":where"
  val DEFAULT_ELSE = ":else"
  val DEFAULT_IF = ":if"
  val DEFAULT_PLACEHOLDER = ":placeholder"

final class DefaultReservedTokens:
  import DefaultReservedTokens._

  val tokens = Map(
    MACRO -> "#",
    COMMENT -> "//",
    SEMI -> ";",
    COLON -> ":",
    WHERE -> DEFAULT_WHERE,
    ELSE -> DEFAULT_ELSE,
    COLONCOLON -> "::",
    EQUALS -> "=",
    GTLT -> "><",
    LFUNNEL -> "-<",
    RFUNNEL -> ">-",
    QUEST -> "?",
    IF -> DEFAULT_IF,
    PLACEHOLDER -> DEFAULT_PLACEHOLDER,
    AT -> "@",
    QUESTCOLON -> "?:",
    ATCOLON -> "@:",
    LARROW -> "<-",
    PERIODPERIOD -> "..",
    PERIOD -> ".",
    RARROW -> "->",
    DOLLAR -> "$"
  )

  val reserved = Map(
    tokens(MACRO) -> Macro,
    tokens(COMMENT) -> Comment,
    tokens(SEMI) -> Semi,
    tokens(COLON) -> Colon,
    tokens(WHERE) -> Where,
    tokens(ELSE) -> Else,
    tokens(COLONCOLON) -> ColonColon,
    tokens(EQUALS) -> Equals,
    tokens(GTLT) -> GtLt,
    tokens(LFUNNEL) -> LFunnel,
    tokens(RFUNNEL) -> RFunnel,
    tokens(QUEST) -> Quest,
    tokens(IF) -> If,
    tokens(PLACEHOLDER) -> Placeholder,
    tokens(AT) -> At,
    tokens(QUESTCOLON) -> QuestColon,
    tokens(ATCOLON) -> AtColon,
    tokens(LARROW) -> LArrow,
    tokens(PERIODPERIOD) -> PeriodPeriod,
    tokens(PERIOD) -> Period,
    tokens(RARROW) -> RArrow,
    tokens(DOLLAR) -> Dollar,
    "(" -> LParen,
    ")" -> RParen,
    "{" -> LBrace,
    "}" -> RBrace,
    "[" -> LBrack,
    "]" -> RBrack
  )

  inline def isReserved: String => Boolean =
    reserved.contains

  def isParenthesis: Char => Boolean =
    "()[]{}\"".contains

  def isSymbol: Char => Boolean =
    "!#$%&'*+,-./:;<=>?@\\^`|~".contains

  def isNumber: Char => Boolean =
    "0123456789".contains

  def isCharacterOrNumber(c: Char): Boolean =
    isCharacter(c) || isNumber(c)

  def isCharacter(c: Char): Boolean =
    List(
      isParenthesis,
      isSymbol,
      isNumber,
      Character.isWhitespace
    ).forall(!_(c))
