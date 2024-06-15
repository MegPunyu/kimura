package tokyo.meg.script.parser.ast

final case class AccessApply(val node: Node, val member: Node) extends Node
