package tokyo.meg.script.parser.ast

final case class ConstInt(override val value: Long) extends Const(value)
