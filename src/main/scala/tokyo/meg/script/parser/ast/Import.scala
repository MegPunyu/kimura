package tokyo.meg.script.parser.ast

final case class Import(val names: Array[String]) extends Node
