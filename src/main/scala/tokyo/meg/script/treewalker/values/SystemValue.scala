package tokyo.meg.script.treewalker.values

final case class SystemValue() extends Value:
  outer = RootValue.singletonRoot
  parent = SystemRoot.singletonRoot
