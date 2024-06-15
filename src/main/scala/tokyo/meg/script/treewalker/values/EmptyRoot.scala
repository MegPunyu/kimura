package tokyo.meg.script.treewalker.values

object EmptyRoot extends HasSingletonRoot[EmptyRoot]:
  val singletonRoot = EmptyRoot()

  initialize()

private final case class EmptyRoot() extends Value
