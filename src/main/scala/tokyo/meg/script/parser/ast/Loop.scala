package tokyo.meg.script.parser.ast

final case class Loop(
    val condition: Node,
    val consequence: Node,
    val alternative: Node
) extends Node
