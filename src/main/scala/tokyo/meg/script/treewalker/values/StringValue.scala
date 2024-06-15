package tokyo.meg.script.treewalker.values

final case class StringValue(val value: String) extends Value:
  outer = RootValue.singletonRoot
  parent = StringRoot.singletonRoot

  setBody(ArrayValue.singletonRoot.concat(this).body)

  override def toString(): String =
    value

implicit inline final def toStringValue(value: String | Char): StringValue =
  StringValue(value.toString())
