package tokyo.meg.script.parser.ast

final case class Lambda(val argument: String, val body: Node) extends Node
