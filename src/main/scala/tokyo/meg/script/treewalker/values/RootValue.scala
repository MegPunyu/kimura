package tokyo.meg.script.treewalker.values

import scala.util.chaining._

object RootValue extends HasSingletonRoot[RootValue]:
  val singletonRoot = RootValue()

  initialize()

  import Value._

  singletonRoot.attributes.addAll(
    Array(
      "Root" -> RootValue.singletonRoot,
      "Number" -> NumberValue.singletonRoot,
      "Int" -> IntRoot.singletonRoot,
      "Real" -> RealRoot.singletonRoot,
      "Array" -> ArrayValue.singletonRoot,
      "String" -> StringRoot.singletonRoot,
      "List" -> ListRoot.singletonRoot,
      "Empty" -> EmptyRoot.singletonRoot,
      "Reader" -> ReaderRoot.singletonRoot,
      "Writer" -> WriterRoot.singletonRoot,
      "System" -> SystemRoot.singletonRoot,
      "true" -> IntValue(1),
      "false" -> IntValue(0),
      "instanceOf" -> function(a => function(b => instanceOf(a, b))),
      "input" -> input,
      "println" -> _println,
      "print" -> _print,
      "is" -> is,
      "toString" -> function(_.toString().pipe(StringValue(_))),
      "==" -> is,
      ".:" -> compose,
      ":." -> andThen,
      "-<<" -> applyToLeft,
      ">>-" -> applyToRight
    )
  )

  def is: Value = biFunction: (a, b) =>
    IntValue(if (a eq b) 1 else 0)

  @annotation.tailrec
  def instanceOf(a: Value, b: Value): Value =
    if a.eq(b) || b.eq(RootValue.singletonRoot)
    then IntValue(1)
    else
      a match
        case RootValue() => IntValue(0)
        case _           => instanceOf(a.parent, b)

  def input: Value = function: message =>
    print(message)

    ReaderRoot.singletonRoot.getLine(ReaderValue(() => System.in, "UTF-8"))

  def _println: Value =
    WriterRoot.singletonRoot._println(WriterValue(() => System.out, "UTF-8"))

  def _print: Value =
    WriterRoot.singletonRoot._print(WriterValue(() => System.out, "UTF-8"))

  def compose: Value = biFunction: (a, b) =>
    function(a.body.compose(b.body))

  def andThen: Value = biFunction: (a, b) =>
    function(a.body.andThen(b.body))

  def applyToLeft: Value = biFunction: (a, b) =>
    a body b

  def applyToRight: Value = biFunction: (a, b) =>
    b body a

final private case class RootValue() extends Value
