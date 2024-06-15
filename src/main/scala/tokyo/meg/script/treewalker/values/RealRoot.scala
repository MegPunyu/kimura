package tokyo.meg.script.treewalker.values

import scala.util.chaining._

object RealRoot extends HasSingletonRoot[RealRoot]:
  val singletonRoot = RealRoot()

  initialize(NumberValue)

  NumberValue.singletonRoot.attributes.addAll(
    Array(
      "PI" -> RealValue(Math.PI),
      "E" -> RealValue(Math.E),
      "TAU" -> RealValue(Math.TAU)
    )
  )

final private case class RealRoot() extends Value:
  import Value._

  setBody[RealValue]:
    case IntValue(value)     => value.toDouble
    case RealValue(value)    => value
    case StringValue(values) => toRealValue(values)
    case _                   => 0
