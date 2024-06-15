package tokyo.meg.script.treewalker.values

final case class IntValue(val value: Long) extends Value:
  outer = RootValue.singletonRoot
  parent = IntRoot.singletonRoot

  setBody(NumberValue.singletonRoot.mul(this).body)

  override def toString(): String =
    value.toString()

implicit inline final def toIntValue(value: Boolean): IntValue =
  if (value) 1 else 0

implicit inline final def toIntValue(value: Long | Int): IntValue =
  value match
    case value: Long => IntValue(value)
    case value: Int  => IntValue(value)

inline final def toIntValue(value: String): IntValue =
  try java.lang.Long.parseLong(value)
  catch _ => 0

inline final def toIntValue(value: Value): IntValue =
  value match
    case IntValue(a)    => a
    case RealValue(a)   => a.toLong
    case StringValue(a) => toIntValue(a)
    case _              => 0
