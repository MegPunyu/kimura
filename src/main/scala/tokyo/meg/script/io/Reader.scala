package tokyo.meg.script.io

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.nio.file.Paths

import scala.util._
import scala.util.chaining._

object Reader:
  def open[T](stream: InputStream): (Reader => T) => Try[T] =
    open(() => Reader(stream))

  def open[T, U <: Reader](readerGenerator: () => U)(f: U => T): Try[T] =
    var reader: Option[U] = None

    Try:
      reader = Some(readerGenerator())
      f(reader.get)
    .tap: _ =>
      reader match
        case Some(reader) => reader.close()
        case None         => ()

class Reader(val stream: InputStream):
  private val reader = BufferedReader(InputStreamReader(stream))

  def readLine(): Option[String] =
    reader.readLine() match
      case null => None
      case line => Some(line)

  def close(): Unit =
    reader.close()
