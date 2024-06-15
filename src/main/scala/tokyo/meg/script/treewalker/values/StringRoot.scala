package tokyo.meg.script.treewalker.values

import scala.collection.mutable._
import scala.util.chaining._

object StringRoot extends HasSingletonRoot[StringRoot]:
  val singletonRoot = StringRoot()

  initialize(ArrayValue)

final private case class StringRoot() extends Value:
  import Value._

  attributes.addAll(
    Array(
      "<" -> lt,
      ">" -> gt,
      "<=" -> le,
      ">=" -> ge,
      "==" -> _eq,
      "split" -> split,
      "replace" -> replace,
      "contains" -> contains,
      "test" -> test,
      "toList" -> toList,
      "toCharCodes" -> toCharCodes,
      "fromCharCodes" -> fromCharCodes
    )
  )

  private val I = IntValue
  private val L = ListValue
  private val R = RealValue
  private val S = StringValue

  def lt: Value = biFunction:
    case (S(a), b) => a < b._toString
    case _         => EmptyValue()

  def gt: Value = biFunction:
    case (S(a), b) => a > b._toString
    case _         => EmptyValue()

  def le: Value = biFunction:
    case (S(a), b) => a <= b._toString
    case _         => EmptyValue()

  def ge: Value = biFunction:
    case (S(a), b) => a >= b._toString
    case _         => EmptyValue()

  def _eq: Value = biFunction:
    case (S(a), b) => a == b._toString
    case _         => EmptyValue()

  def split: Value = biFunction:
    case (S(a), b) => a.split(b._toString).map(S(_))
    case _         => EmptyValue()

  def replace: Value = triFunction:
    case (S(a), b, c) => a.replaceAll(b._toString, c._toString)
    case _            => EmptyValue()

  def contains: Value = biFunction:
    case (S(a), b) => a.contains(b._toString)
    case _         => EmptyValue()

  def test: Value = biFunction:
    case (S(a), b) => a.matches(b._toString)
    case _         => EmptyValue()

  def toList: Value = function:
    case S(a) => a.toArray.map(_.toString()).map(StringValue(_))
    case _    => EmptyValue()

  def toCharCodes: Value = function:
    case S(a) => a.toArray.map(_.toLong).map(IntValue(_))
    case _    => EmptyValue()

  def fromCharCodes: Value = function:
    case I(a) => a.toChar.toString()
    case R(a) => a.toChar.toString()
    case L(a) => String(a.map(toIntValue).map(_.value.toChar).toArray)
    case _    => EmptyValue()
