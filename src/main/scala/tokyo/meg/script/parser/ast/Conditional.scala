package tokyo.meg.script.parser.ast

final case class Conditional(
    val condition: Node,
    val consequence: Node,
    val alternative: Node
) extends Node
