package tokyo.meg.script.treewalker.values

import java.io._
import java.nio.charset._

import scala.util.chaining._

final case class ReaderValue(
    val streamOpener: () => InputStream,
    val encoding: String
) extends Value:
  outer = RootValue.singletonRoot
  parent = ReaderRoot.singletonRoot

  private val charset = Charset.forName(encoding)
  private var _stream: Option[InputStream] = None
  private var _charStream: Option[BufferedReader] = None

  def stream: InputStream =
    _stream match
      case None         => streamOpener().tap(s => _stream = Some(s))
      case Some(stream) => stream

  def charStream: BufferedReader =
    _charStream match
      case None         => BufferedReader(InputStreamReader(stream, charset))
      case Some(stream) => stream
