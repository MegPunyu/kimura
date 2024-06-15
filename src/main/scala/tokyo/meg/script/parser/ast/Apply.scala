package tokyo.meg.script.parser.ast

final case class Apply(val left: Node, val right: Node) extends Node
