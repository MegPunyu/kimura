package tokyo.meg.script.parser.ast

final case class Access(val node: Node, val member: Node) extends Node
