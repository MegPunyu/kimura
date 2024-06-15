package tokyo.meg.script.lexer

import scala.util.chaining._

import tokyo.meg.script.io._

final class Cursor(private val reader: FileReader) extends Iterator[Char]:
  val parentPath = reader.parentPath

  private var _currentLine: Option[String] = Some("")
  private var _atEof = false
  private var (_line, _column) = (0, 1)
  private var (_currentChar, _nextChar) = ('\n', '\n')

  advance()

  override inline def hasNext: Boolean =
    !atEof

  override inline def next(): Char =
    currentChar tap advance

  inline def currentLine: Option[String] =
    _currentLine

  inline def atEof: Boolean =
    _atEof

  inline def line: Int =
    _line

  inline def column: Int =
    _column

  inline def currentChar: Char =
    _currentChar

  inline def nextChar: Char =
    _nextChar

  @annotation.tailrec
  def advance[T](value: T = this): T =
    _currentChar = _nextChar

    currentLine match
      case None =>
        _nextChar = '\u0000'
        _atEof = 0 < column
        _column += 1

        value

      case Some(string) =>
        if string.length == column - 1
        then fetchLine().advance(value)
        else
          _nextChar = if (string.length == column) '\n' else string(column)

          _column += 1

          value

  private def fetchLine[T](value: T = this): T =
    _currentLine = reader.readLine()
    _column = 0

    currentLine match
      case Some(nextLine) =>
        _line += 1

        if nextLine.filter(_.isWhitespace) == nextLine
        then fetchLine(value)
        else value

      case _ => value
