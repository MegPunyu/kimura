package tokyo.meg.script.parser.ast

final case class Merge(val left: Node, val right: Node) extends Node
