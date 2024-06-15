package tokyo.meg.script.lexer

import scala.util.chaining._
import scala.collection.mutable._

import tokyo.meg.script.util._

final class Lexer(private val cursor: Cursor, var path: String = ".")
    extends Iterator[Token]:

  import cursor._
  import TokenCategory._
  import DefaultReservedTokens._

  val reservedTokens = DefaultReservedTokens()
  import reservedTokens._

  private val levels = Stack[Int](0)
  private var level = 0
  private var atEof = false

  override inline def hasNext: Boolean =
    !atEof

  override inline def next(): Token =
    getToken()

  def getToken(): Token =
    if (level < levels.top) unindentOrOtherToken()
    else if (currentChar == '\n') indentOrOtherToken()
    else eatWhiteSpaces().nonIndentTokens() tap fetchChar tap eatWhiteSpaces

  private def indentOrOtherToken(): Token =
    level = readWhile(_ == ' ').length - 1 tap fetchChar

    if levels.top < level
    then newToken(" " * level, Indent).tap(_ => levels.push(level))
    else getToken()

  private def unindentOrOtherToken(): Token =
    if levels.size == 1 then
      levels.update(0, level)

      getToken()
    else
      levels.pop()

      if (levels.top < level) levels.update(0, level)

      newToken(" " * level, Unindent)

  private def nonIndentTokens(): Token =
    currentChar match
      case '\u0000' =>
        atEof = true

        newToken("\u0000", Eof)

      case prefix @ '-' =>
        if isNumber(nextChar)
        then fetchChar().number(prefix)
        else declarableOrReserved()

      case '"' =>
        string()

      case c =>
        if (c == ':' && isCharacter(nextChar)) identifierOperator()
        else if (isParenthesis(c)) parenthesis()
        else if (isNumber(c)) number()
        else declarableOrReserved()

  private def number(prefix: String | Char = ""): Token =
    val int = prefix.toString() + readWhile(isNumber)

    if nextChar == '.'
    then fetchChar().newToken(int + readWhile(isNumber), LiteralReal)
    else newToken(int, LiteralInt)

  private def string(): Token =
    buildStringWhile(!("\"\n" contains nextChar)):
      fetchChar()

      if currentChar == '\\' then
        fetchChar()

        currentChar match
          case 'n' => '\n'
          case c   => c
      else currentChar
    .append:
      if nextChar == '\n'
      then '\n'
      else "" tap fetchChar
    .pipe(newToken(LiteralStr))

  private def identifierOperator(): Token =
    fetchChar().readWhile(isCharacterOrNumber) pipe newToken(IdentifierOperator)

  private def declarableOrReserved(): Token =
    val (value, category) =
      if isCharacter(currentChar)
      then (readWhile(isCharacterOrNumber), Identifier)
      else (readWhile(isSymbol), Operator)

    reserved get value match
      case Some(Comment)  => commentToken()
      case Some(Macro)    => macroToken()
      case Some(category) => reservedToken(value, category)
      case _              => newToken(value, category)

  private def commentToken(): Token =
    readWhile(_ != '\n') drop 1 pipe newToken(Comment)

  private def macroToken(): Token =
    readWhile(_ != '\n') drop 1 tap execMacro pipe newToken(Macro)

  private def execMacro(value: String): Unit =
    val command = value.trim().split("\\s+")

    if (command.length == 0) return

    command(0).toUpperCase() match
      case "TOKEN"   => execTokenMacro(command)
      case "PACKAGE" => execPackageMacro()

  private def execTokenMacro(command: Array[String]): Unit =
    if (command.length < 3) return

    val (name, value) = (command(1), command(2))

    if tokens.contains(name) && !reserved.contains(value) then
      reserved.update(value, reserved(tokens(name)))
      reserved.remove(tokens(name))
      tokens.update(name, value)

  private def execPackageMacro(): Unit =
    path = parentPath

  private def reservedToken(value: String, category: TokenCategory): Token =
    newToken(value, if (isCanceled(category)) Operator else category)

  private def isCanceled(category: TokenCategory): Boolean =
    if category == Colon then
      tokens(WHERE) != DEFAULT_WHERE && tokens(ELSE) != DEFAULT_ELSE
    else if category == Quest then
      tokens(IF) != DEFAULT_IF && tokens(PLACEHOLDER) != DEFAULT_PLACEHOLDER
    else false

  private def parenthesis(): Token =
    newToken(reserved)(currentChar)

  private def readWhile(f: Char => Boolean): String =
    buildStringWhile(currentChar)(f(nextChar)):
      nextChar tap fetchChar
    .toString()

  private def newToken(value: Any, category: TokenCategory): Token =
    Token(line, column - 1)(value.toString(), category)

  private def newToken(category: String => TokenCategory): Any => Token =
    value => newToken(value, category(value.toString()))

  private def newToken(category: TokenCategory): Any => Token =
    newToken(_ => category)

  @annotation.tailrec
  private def eatWhiteSpaces[T](value: T = this): T =
    if currentChar != '\n' && currentChar.isWhitespace
    then fetchChar().eatWhiteSpaces(value)
    else value

  private def fetchChar[T](value: T = this): T =
    value tap advance
