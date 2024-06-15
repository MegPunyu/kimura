package tokyo.meg.script.treewalker.values

import java.io._

import scala.util.chaining._

object ReaderRoot extends HasSingletonRoot[ReaderRoot]:
  val singletonRoot = ReaderRoot()

  initialize()

private final case class ReaderRoot() extends Value:
  import Value._

  setBody: path =>
    (() => FileInputStream(File(path._toString))).pipe: stream =>
      ReaderValue(stream, "UTF-8").setBody: encoding =>
        ReaderValue(stream, encoding._toString)

  attributes.addAll(
    Array(
      "read" -> read,
      "getChar" -> getChar,
      "getLine" -> getLine,
      "close" -> close
    )
  )

  def read: Value =
    input(_.stream.read().toLong pipe IntValue.apply)

  def getChar: Value = input:
    _.charStream
      .read()
      .pipe(Array(_))
      .pipe(c => String(c, 0, c.length))

  def getLine: Value =
    input(_.charStream.readLine())

  private def input(f: ReaderValue => Value): Value = function:
    case reader @ ReaderValue(_, _) =>
      try f(reader)
      catch _ => EmptyValue()

    case _ => EmptyValue()

  private def close: Value = function:
    case reader @ ReaderValue(_, _) => reader tap (_ => reader.stream.close())
    case _                          => EmptyValue()
