package tokyo.meg.script.treewalker.values

import java.io._
import java.nio.charset._

import scala.util.chaining._

object WriterRoot extends HasSingletonRoot[WriterRoot]:
  val singletonRoot = WriterRoot()

  initialize()

private final case class WriterRoot() extends Value:
  import Value._

  setBody: path =>
    val file = File(path._toString.toString())
    val F: (File, Boolean) => OutputStream = FileOutputStream(_, _)
    val W = WriterValue

    W(() => F(file, false), "UTF-8").setBody: append =>
      W(() => F(file, append.isTruthy), "UTF-8").setBody: encoding =>
        W(() => F(file, append.isTruthy), encoding._toString.toString())

  attributes.addAll(
    Array(
      "println" -> _println,
      "print" -> _print,
      "write" -> write
    )
  )

  def _println: Value =
    output(writer => value => writer.charStream.println(value._toString))

  def _print: Value =
    output(writer => value => writer.charStream.print(value._toString))

  def write: Value =
    output: writer =>
      case IntValue(value)    => writer.stream.write(value.toInt)
      case RealValue(value)   => writer.stream.write(value.toInt)
      case StringValue(value) => writer.stream.write(value.getBytes())
      case ListValue(value)   => value foreach writer(writer).body
      case _                  => ()

  private def output(f: WriterValue => Value => Any): Value = function:
    case writer @ WriterValue(_, _) =>
      def g: Value = function:
        _.tap:
          try f(writer)(_)
          catch e => _ => ()
        .pipe(_ => g)

      g

    case _ => EmptyValue()

  private def close: Value = function:
    case writer: WriterValue => writer.tap(_ => writer.stream.close())
    case _                   => EmptyValue()
