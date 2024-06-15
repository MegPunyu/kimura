package tokyo.meg.script.parser.ast

final case class ConstString(override val value: String) extends Const(value)
