package tokyo.meg.script.parser.ast

final case class ConstReal(override val value: Double) extends Const(value)
