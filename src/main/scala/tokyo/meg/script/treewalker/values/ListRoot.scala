package tokyo.meg.script.treewalker.values

import scala.util.chaining._

object ListRoot extends HasSingletonRoot[ListRoot]:
  val singletonRoot = ListRoot()

  initialize(ArrayValue)

final private case class ListRoot() extends Value:
  import Value._

  attributes.addAll(
    Array(
      "swap" -> swap,
      "push" -> push,
      "unshift" -> unshift,
      "insert" -> insertElement,
      "remove" -> removeElement,
      "pop" -> pop,
      "shift" -> shift,
      "sort" -> sort,
      ">+" -> push,
      "<+" -> unshift,
      "++" -> insertElement,
      "--" -> removeElement
    )
  )

  private val L = ListValue
  private val I = IntValue
  private val R = RealValue

  private def swap(self: ListValue, i: Int, j: Int): Value =
    val index = ArrayValue.singletonRoot.index(self.values)(_)

    self.tap:
      _.values(index(i)).pipe: e =>
        self.values.update(index(i), self.values(index(j)))
        self.values.update(index(j), e)

  def swap: Value = triFunction:
    case (self @ L(_), I(i), I(j)) => swap(self, i.toInt, j.toInt)
    case (self @ L(_), I(i), R(j)) => swap(self, i.toInt, j.toInt)
    case (self @ L(_), R(i), I(j)) => swap(self, i.toInt, j.toInt)
    case (self @ L(_), R(i), R(j)) => swap(self, i.toInt, j.toInt)
    case _                         => EmptyValue()

  def push: Value = biFunction:
    case (self @ L(values), value) => self.tap(_ => values.append(value))
    case _                         => EmptyValue()

  def unshift: Value = biFunction:
    case (self @ L(values), value) => self.tap(_ => values.prepend(value))
    case _                         => EmptyValue()

  def pop: Value = function:
    case L(values) => values.remove(-1)
    case _         => EmptyValue()

  def shift: Value = function:
    case L(values) => values.remove(0)
    case _         => EmptyValue()

  def insertElement: Value = triFunction:
    case (self @ L(_), I(i), value) => self.insert(i.toInt, value)
    case (self @ L(_), R(i), value) => self.insert(i.toInt, value)
    case _                          => EmptyValue()

  def removeElement: Value = biFunction:
    case (L(values), I(i)) => values.remove(i.toInt)
    case (L(values), R(i)) => values.remove(i.toInt)
    case _                 => EmptyValue()

  def sort: Value = biFunction:
    case (self @ L(_), f) => self.sort((a, b) => f(a)(b).isTruthy)
    case _                => EmptyValue()
