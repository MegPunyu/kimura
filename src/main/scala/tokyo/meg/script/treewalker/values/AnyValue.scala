package tokyo.meg.script.treewalker.values

import RootValue._

final case class AnyValue(_outer: Value = singletonRoot) extends Value:
  outer = _outer
  parent = singletonRoot