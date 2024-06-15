package tokyo.meg.script.treewalker.values

final case class RealValue(val value: Double) extends Value:
  outer = RootValue.singletonRoot
  parent = RealRoot.singletonRoot

  setBody(NumberValue.singletonRoot.mul(this).body)

  override def toString(): String =
    value.toString()

implicit inline final def toRealValue(value: Double): RealValue =
  RealValue(value)

inline final def toRealValue(value: String): RealValue =
  try java.lang.Double.parseDouble(value)
  catch _ => 0
