package tokyo.meg.script.treewalker.values

import scala.collection.mutable._
import scala.util.chaining._

final case class ListValue(val values: ArrayBuffer[Value]) extends Value:
  import ArrayValue.singletonRoot._

  outer = RootValue.singletonRoot
  parent = ListRoot.singletonRoot

  setBody(ArrayValue.singletonRoot.at(this).body)

  def insert(i: Int, value: Value): Value =
    this.tap(_ => values.insert(index(values.length, i), value))

  def sort(f: (Value, Value) => Boolean): Value =
    this.tap(_ => values.sortInPlaceWith(f))

  override def toString(): String =
    val seq = values
      .map: e =>
        if e eq this
        then "[...]"
        else
          e match
            case StringValue(e) => s"\"${e}\""
            case _              => e._toString
      .mkString("; ")

    s"[${seq}]"

implicit inline final def toListValue(value: ArrayBuffer[Value]): ListValue =
  ListValue(value)

implicit inline final def toListValue[T <: Value](value: Array[T]): ListValue =
  ListValue(ArrayBuffer(value*))

implicit inline final def toListValue[T <: Value](value: Seq[T]): ListValue =
  value.toArray[Value]
