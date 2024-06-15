package tokyo.meg.script.treewalker.values

import scala.collection.mutable._
import scala.util.chaining._

abstract class Value:
  var parent: Value = null
  var outer: Value = null
  var body: Value => Value = _ => this
  val attributes: Map[String, Value] = Map()

  def setBody[T <: Value](value: Value => T): Value =
    this.tap(_.body = value)

  def get: (Value => Value) => String => Value =
    operate(_ => value => _ => value, _ => _ => EmptyValue())

  infix def define(value: Value)(name: String): Value =
    value.tap(_ => attributes.update(name, value))

  def remove: (Value => Value) => String => Value =
    operate(self => _ => self.remove, _ => _ => EmptyValue())

  def remove(name: String): Value =
    attributes.remove(name) match
      case Some(value) => value
      case None        => EmptyValue()

  private def operate(
      ifExists: Value => Value => String => Value,
      ifRoot: Value => String => Value
  )(target: Value => Value)(name: String): Value =
    attributes get name match
      case Some(value) => ifExists(this)(value)(name)
      case None =>
        this match
          case RootValue() => ifRoot(this)(name)
          case _ =>
            target(this) match
              case null  => EmptyValue()
              case value => value.operate(ifExists, ifRoot)(target)(name)

  final def isTruthy: Boolean =
    !(this(this) eq this)

  final def apply(value: Value): Value =
    this body value

  final def method: String => Value =
    this.get(_.parent)(_)(this)

  def toStringValue: Value =
    method("toString")

  def _toString: String =
    toStringValue.toString()

  def ==(other: Value): Boolean =
    method("==")(other).isTruthy

  override def toString(): String =
    if attributes.size == 0
    then "{}"
    else
      attributes
        .map((name, value) =>
          (value match
            case StringValue(value) => s"\"$value\""
            case _                  => if (value eq this) name else value
          ).pipe(s => s"$name = $s")
        )
        .mkString("; ")
        .pipe(s => s"{ $s }")

object Value:
  final def function(body: Value => Value): Value =
    AnyValue().tap(_.setBody(body))

  final def biFunction(f: (Value, Value) => Value): Value =
    function(left => function(f(left, _)))

  final def triFunction(f: (Value, Value, Value) => Value): Value =
    function(left => biFunction(f(left, _, _)))
