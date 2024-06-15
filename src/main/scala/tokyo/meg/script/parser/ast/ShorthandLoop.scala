package tokyo.meg.script.parser.ast

final case class ShorthandLoop(
    val condition: Node,
    val alternative: Node
) extends Node
