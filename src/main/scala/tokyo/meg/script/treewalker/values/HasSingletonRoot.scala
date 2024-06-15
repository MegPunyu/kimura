package tokyo.meg.script.treewalker.values

import scala.util.chaining._

trait HasSingletonRoot[T <: Value]:
  val singletonRoot: T

  final def initialize[U <: Value](
      Root: HasSingletonRoot[U] = RootValue
  ): Unit =
    singletonRoot.parent = Root.singletonRoot
    singletonRoot.outer = Root.singletonRoot
