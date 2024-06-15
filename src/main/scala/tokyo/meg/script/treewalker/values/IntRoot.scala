package tokyo.meg.script.treewalker.values

import scala.util.chaining._

object IntRoot extends HasSingletonRoot[IntRoot]:
  val singletonRoot = IntRoot()

  initialize(NumberValue)

final private case class IntRoot() extends Value:
  import Value._

  setBody[IntValue]:
    case IntValue(value)     => value
    case RealValue(value)    => value.toLong
    case StringValue(values) => toIntValue(values)
    case _                   => 0
