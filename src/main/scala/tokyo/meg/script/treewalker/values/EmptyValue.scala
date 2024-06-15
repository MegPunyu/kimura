package tokyo.meg.script.treewalker.values

final case class EmptyValue(val isImplicit: Boolean = false) extends Value:
  outer = RootValue.singletonRoot
  parent = EmptyRoot.singletonRoot

  override def toString(): String =
    "()"
