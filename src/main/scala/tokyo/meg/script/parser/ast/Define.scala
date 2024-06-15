package tokyo.meg.script.parser.ast

final case class Define(val name: String, val value: Node) extends Node
