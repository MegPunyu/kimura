package tokyo.meg.script.parser.ast

final case class Sequence(val nodes: Array[Node]) extends Node
