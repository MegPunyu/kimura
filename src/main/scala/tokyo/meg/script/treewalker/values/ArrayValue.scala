package tokyo.meg.script.treewalker.values

import scala.collection.mutable._
import scala.util.chaining._

object ArrayValue extends HasSingletonRoot[ArrayValue]:
  val singletonRoot = ArrayValue()

  initialize()

final private case class ArrayValue() extends Value:
  import Value._

  attributes.addAll(
    Array(
      "*" -> repeat,
      "+" -> concat,
      "-<>" -> indexOf,
      "<>-" -> lastIndexOf,
      "at" -> at,
      "len" -> len,
      "slice" -> slice,
      "reversed" -> reversed,
      "sorted" -> sorted,
      "indexOf" -> indexOf,
      "lastIndexOf" -> lastIndexOf,
      "indexWhere" -> indexWhere,
      "lastIndexWhere" -> lastIndexWhere,
      "map" -> map,
      "filter" -> filter,
      "reduce" -> reduce,
      "all" -> all,
      "any" -> any,
      "take" -> take,
      "takeRight" -> takeRight,
      "takeWhile" -> takeWhile,
      "drop" -> drop,
      "dropRight" -> dropRight,
      "dropWhile" -> dropWhile,
      "join" -> join
    )
  )

  private val I = IntValue
  private val R = RealValue
  private val L = ListValue
  private val S = StringValue

  private def s: Any => Value =
    case value: Value => value.toStringValue
    case value        => value.toString()

  def index(length: Int, i: Int): Int =
    try (i % length + length) % length
    catch _ => 0

  def index[T](a: Seq[T]): Int => Int =
    index(a.length, _)

  def index(a: String): Int => Int =
    index(a.length, _)

  def getValue(a: Seq[Value], i: Int): Value =
    try a(index(a)(i))
    catch _ => EmptyValue()

  def getValue(a: String, i: Int): Value =
    try a(index(a)(i))
    catch _ => ""

  def at: Value =
    biFunction:
      try
        (_, _) match
          case (S(values), I(i))       => getValue(values, i.toInt)
          case (S(values), R(i))       => getValue(values, i.toInt)
          case (L(values), I(i))       => getValue(values, i.toInt)
          case (L(values), R(i))       => getValue(values, i.toInt)
          case (L(values), L(indices)) => indices map body
          case _                       => EmptyValue()
      catch _ => (_, _) => EmptyValue()

  def len: Value = function:
    case S(values) => I(values.length)
    case L(values) => I(values.length)
    case _         => EmptyValue()

  def slice: Value = triFunction:
    case (S(values), I(a), I(b)) => values.slice(a.toInt, b.toInt)
    case (S(values), I(a), R(b)) => values.slice(a.toInt, b.toInt)
    case (S(values), R(a), I(b)) => values.slice(a.toInt, b.toInt)
    case (S(values), R(a), R(b)) => values.slice(a.toInt, b.toInt)
    case (L(values), I(a), I(b)) => values.slice(a.toInt, b.toInt)
    case (L(values), I(a), R(b)) => values.slice(a.toInt, b.toInt)
    case (L(values), R(a), I(b)) => values.slice(a.toInt, b.toInt)
    case (L(values), R(a), R(b)) => values.slice(a.toInt, b.toInt)
    case _                       => EmptyValue()

  def reversed: Value = function:
    case S(values) => values.reverse
    case L(values) => L(values.reverse)
    case _         => EmptyValue()

  def sorted: Value = biFunction:
    case (S(values), f) => values.sortWith((a, b) => f(a)(b).isTruthy)
    case (L(values), f) => values.sortWith((a, b) => f(a)(b).isTruthy)
    case _              => EmptyValue()

  def repeat: Value = biFunction:
    case (S(a), I(b)) => a.repeat(b.toInt)
    case (S(a), R(b)) => a.repeat(b.toInt)
    case (L(a), I(b)) => ArrayBuffer.fill(b.toInt)(a).flatten
    case (L(a), R(b)) => ArrayBuffer.fill(b.toInt)(a).flatten
    case _            => EmptyValue()

  def concat: Value = biFunction:
    case (S(a), S(b))     => a + b
    case (S(a), b)        => a + b._toString
    case (L(a), L(b))     => a concat b
    case (list @ L(a), b) => list._toString + b._toString
    case _                => EmptyValue()

  def indexOf: Value = biFunction:
    case (S(a), b) => a.indexOf(b._toString)
    case (L(a), b) => a indexWhere b.==
    case _         => EmptyValue()

  def lastIndexOf: Value = biFunction:
    case (S(a), b) => a.lastIndexOf(b._toString)
    case (L(a), b) => a lastIndexWhere b.==
    case _         => EmptyValue()

  def indexWhere: Value = biFunction:
    case (S(a), b) => a map s indexWhere b.body.andThen(_.isTruthy)
    case (L(a), b) => a indexWhere b.body.andThen(_.isTruthy)
    case _         => EmptyValue()

  def lastIndexWhere: Value = biFunction:
    case (S(a), b) => a map s lastIndexWhere b.body.andThen(_.isTruthy)
    case (L(a), b) => a lastIndexWhere b.body.andThen(_.isTruthy)
    case _         => EmptyValue()

  def map: Value = biFunction:
    case (S(values), f) => values.toArray.map(f(_)).mkString("")
    case (L(values), f) => values map f.body
    case _              => EmptyValue()

  def filter: Value = biFunction:
    case (S(values), f) => values.filter(f(_).isTruthy)
    case (L(values), f) => values.filter(e => f(e).isTruthy)
    case _              => EmptyValue()

  def reduce: Value = biFunction:
    case (S(values), f) => values.toArray.map(s).reduce((a, b) => f(a)(b))
    case (L(values), f) => values.reduce((a, b) => f(a)(b))
    case _              => EmptyValue()

  def all: Value = biFunction:
    case (S(values), f) => values.forall(f(_).isTruthy)
    case (L(values), f) => values.forall(f(_).isTruthy)
    case _              => EmptyValue()

  def any: Value = biFunction:
    case (S(values), f) => values.exists(f(_).isTruthy)
    case (L(values), f) => values.exists(f(_).isTruthy)
    case _              => EmptyValue()

  def take: Value = biFunction:
    case (S(value), I(n)) => value take n.toInt
    case (S(value), R(n)) => value take n.toInt
    case (L(value), I(n)) => value take n.toInt
    case (L(value), R(n)) => value take n.toInt
    case _                => EmptyValue()

  def takeRight: Value = biFunction:
    case (S(value), I(n)) => value takeRight n.toInt
    case (S(value), R(n)) => value takeRight n.toInt
    case (L(value), I(n)) => value takeRight n.toInt
    case (L(value), R(n)) => value takeRight n.toInt
    case _                => EmptyValue()

  def takeWhile: Value = biFunction:
    case (S(value), f) => value.takeWhile(f(_).isTruthy)
    case (L(value), f) => value.takeWhile(f(_).isTruthy)
    case _             => EmptyValue()

  def drop: Value = biFunction:
    case (S(value), I(n)) => value drop n.toInt
    case (S(value), R(n)) => value drop n.toInt
    case (L(value), I(n)) => value drop n.toInt
    case (L(value), R(n)) => value drop n.toInt
    case _                => EmptyValue()

  def dropRight: Value = biFunction:
    case (S(value), I(n)) => value dropRight n.toInt
    case (S(value), R(n)) => value dropRight n.toInt
    case (L(value), I(n)) => value dropRight n.toInt
    case (L(value), R(n)) => value dropRight n.toInt
    case _                => EmptyValue()

  def dropWhile: Value = biFunction:
    case (S(value), f) => value.dropWhile(f(_).isTruthy)
    case (L(value), f) => value.dropWhile(f(_).isTruthy)
    case _             => EmptyValue()

  def join: Value = biFunction:
    case (S(a), b) => a.map(s).mkString(b._toString)
    case (L(a), b) => a.map(s).mkString(b._toString)
    case _         => EmptyValue()
