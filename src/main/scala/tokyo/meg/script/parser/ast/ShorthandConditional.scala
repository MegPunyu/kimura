package tokyo.meg.script.parser.ast

final case class ShorthandConditional(
    val condition: Node,
    val alternative: Node
) extends Node
