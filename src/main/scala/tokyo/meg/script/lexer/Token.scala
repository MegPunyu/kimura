package tokyo.meg.script.lexer

import scala.collection.mutable._

import TokenCategory._

final class Token(
    val line: Int,
    val column: Int
)(
    val value: String,
    val category: TokenCategory
):

  inline infix def is: TokenCategory => Boolean =
    category == _

  inline def isDeclarable: Boolean =
    category == Operator || category == Identifier

  def isAppliable: Boolean =
    isParenthesisStart || isLiteral || List(
      Identifier,
      Placeholder,
      Equals,
      RArrow,
      Dollar
    ).contains(category)

  def isOperator: Boolean =
    List(Operator, IdentifierOperator) contains category

  def isLiteral: Boolean =
    List(LiteralInt, LiteralReal, LiteralStr) contains category

  def isParenthesisStart: Boolean =
    List(LParen, LBrace, LBrack, Indent) contains category

  def isParenthesisEnd: Boolean =
    List(RParen, RBrace, RBrack, Unindent) contains category

  override inline def toString(): String =
    value
