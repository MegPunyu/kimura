package tokyo.meg.script.treewalker.values

import java.io._
import java.nio.charset._

import scala.util.chaining._

final case class WriterValue(
    val streamOpener: () => OutputStream,
    val encoding: String
) extends Value:
  outer = RootValue.singletonRoot
  parent = WriterRoot.singletonRoot

  private val charset = Charset.forName(encoding)
  private var _stream: Option[OutputStream] = None
  private var _charStream: Option[PrintStream] = None

  def stream: OutputStream =
    _stream match
      case None         => streamOpener().tap(s => _stream = Some(s))
      case Some(stream) => stream

  def charStream: PrintStream =
    _charStream match
      case None         => PrintStream(stream, true, charset)
      case Some(stream) => stream
