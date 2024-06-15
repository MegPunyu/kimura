package tokyo.meg.script.parser.ast

final case class Inherit(val parent: Node, val child: Node) extends Node
