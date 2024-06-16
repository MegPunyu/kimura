package tokyo.meg.script.io

import java.io.{
  BufferedReader,
  File,
  FileReader => JavaFileReader,
  InputStreamReader
}
import java.nio.file.Paths

import scala.util._
import scala.util.chaining._
import java.io.FileInputStream

object FileReader:
  def open[T](path: String): (FileReader => T) => Try[T] =
    Reader.open(() => FileReader(path))

final class FileReader private (val path: String)
    extends Reader(FileInputStream(path)):
  val file = File(path)
  val parentPath = Paths
    .get(file.getAbsolutePath())
    .normalize()
    .getParent()
    .toString()
    .replaceAll("\\\\", "/")

  private val reader = BufferedReader(InputStreamReader(stream))
