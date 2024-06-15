package tokyo.meg.script.parser.ast

final case class AttributeDefine(
    val node: Node,
    val attribute: String,
    val value: Node
) extends Node
