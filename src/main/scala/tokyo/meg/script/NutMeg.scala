package tokyo.meg.script

import java.io.IOException

import scala.util._
import scala.util.chaining._

import tokyo.meg.script.io._
import tokyo.meg.script.util._
import tokyo.meg.script.lexer._
import tokyo.meg.script.parser._
import tokyo.meg.script.treewalker._
import tokyo.meg.script.treewalker.values._

@main def main(args: String*): Unit =
  val path = if (args.length == 0) None else Some(args(0))

  val reader = path match {
    case Some(path) => FileReader.open[Value](path)
    case None       => Reader.open[Value](System.in)
  }

  reader: reader =>
    val cursor = Cursor(reader)
    val lexer = Lexer(cursor)
    val parser = Parser(lexer)
    val ast = parser.parse();
    val treeWalker = TreeWalker((ast, "."))

    treeWalker.eval()
  match {
    case Failure(exception: IOException) =>
      System.err.println(
        path match
          case Some(path) =>
            s"An error occurred while processing the file \"$path\": ${exception.getMessage()}"

          case _ =>
            s"An error occurred while processing the input: ${exception.getMessage()}"
      )

      1

    case Failure(exception: Throwable) =>
      System.err.println(exception)
      System.err.println(
        s"An unexpected error occurred: ${exception.getMessage()}"
      )
      exception.printStackTrace()

      1

    case Success(value) =>
      value match
        case IntValue(value)  => value.toInt
        case RealValue(value) => value.toInt
        case _                => 0

  } pipe System.exit
