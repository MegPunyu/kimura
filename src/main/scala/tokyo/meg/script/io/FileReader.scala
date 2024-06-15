package tokyo.meg.script.io

import java.io.{BufferedReader, File, FileReader => JavaFileReader}
import java.nio.file.Paths

import scala.util._
import scala.util.chaining._

object FileReader:
  def open[T](path: String)(f: FileReader => T): Try[T] =
    var reader: FileReader = null

    Try:
      reader = FileReader(path)
      f(reader)
    .tap: _ =>
      if reader != null then reader.close()

final class FileReader private (val path: String):
  val file = File(path)
  val parentPath = Paths
    .get(file.getAbsolutePath())
    .normalize()
    .getParent()
    .toString()
    .replaceAll("\\\\", "/")

  private val reader = BufferedReader(JavaFileReader(file))

  def readLine(): Option[String] =
    reader.readLine() match
      case null => None
      case line => Some(line)

  def close(): Unit =
    reader.close()
