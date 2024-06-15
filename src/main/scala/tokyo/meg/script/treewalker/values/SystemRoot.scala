package tokyo.meg.script.treewalker.values

import java.io.PrintStream
import java.nio.charset._

import scala.util.chaining._

object SystemRoot extends HasSingletonRoot[SystemRoot]:
  val singletonRoot = SystemRoot()

  initialize()

private final case class SystemRoot() extends Value:
  import Value._

  attributes.addAll(
    Array(
      "in" -> in,
      "out" -> out,
      "err" -> err
    )
  )

  def in: Value =
    ReaderValue(() => System.in, "UTF-8").setBody: encoding =>
      ReaderValue(() => System.in, encoding._toString)

  def out: Value =
    WriterValue(() => System.out, "UTF-8").setBody: encoding =>
      WriterValue(() => System.out, encoding._toString)

  def err: Value =
    WriterValue(() => System.err, "UTF-8").setBody: encoding =>
      WriterValue(() => System.err, encoding._toString)
