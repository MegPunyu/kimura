package tokyo.meg.script.util

import scala.collection.mutable._

@annotation.tailrec
def buildStringWhile(
    builder: StringBuilder
)(condition: => Boolean)(f: => Any): StringBuilder =
  if condition
  then
    builder append f
    buildStringWhile(builder)(condition)(f)
  else builder

def buildStringWhile(condition: => Boolean)(f: => Any): StringBuilder =
  buildStringWhile(StringBuilder())(condition)(f)

def buildStringWhile(initialChar: Any)(condition: => Boolean)(
    f: => Any
): StringBuilder =
  buildStringWhile(StringBuilder() append initialChar)(condition)(f)
