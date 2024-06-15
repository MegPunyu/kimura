package tokyo.meg.script.treewalker

import scala.collection.mutable._
import scala.util._
import scala.util.chaining._

import tokyo.meg.script.io._
import tokyo.meg.script.lexer._
import tokyo.meg.script.parser._
import tokyo.meg.script.parser.ast._
import tokyo.meg.script.treewalker.values._

final class TreeWalker(
    private val module: (Node, String),
    private val modules: Map[String, (Node, String)] = Map(),
    private val environment: Value = AnyValue()
):
  import Value._

  private val (ast, path) = module
  private val environments: Stack[Value] = Stack(environment)

  def eval(): Value =
    ast.$

  private def eval(accessFromParent: Boolean): Node => Value =
    case node: Access                => access(node)
    case node: AccessApply           => accessApply(node)
    case node: Apply                 => applyExpression(node)
    case node: ApplySection          => applySection(node)
    case node: AttributeDefine       => attributeDefine(node)
    case node: AccessApplySection    => accessApplySection(node)
    case node: AccessSection         => accessSection(node)
    case node: Conditional           => conditional(node)
    case node: Define                => variableDefine(node)
    case node: IdentifierSectionLeft => identifierSectionLeft(node)
    case node: Import                => importExpression(node)
    case node: Inherit               => inherit(node)
    case node: Lambda                => lambda(node)
    case node: Loop                  => loop(node, None)
    case node: Merge                 => merge(node)
    case node: ParentOf              => parentOf(node)
    case node: Sequence              => list(node)
    case node: ShorthandConditional  => conditional(node)
    case node: ShorthandLoop         => loop(node, None)
    case node: Statements            => statements(node)
    case node: Struct                => struct(node)
    case node: Variable              => variable(accessFromParent)(node.name)
    case This()                      => env
    case Parent()                    => env.parent
    case Outer()                     => env.outer
    case ConstInt(value)             => IntValue(value)
    case ConstReal(value)            => RealValue(value)
    case ConstString(value)          => StringValue(value)
    case Empty(isImplicit)           => EmptyValue(isImplicit)
    case _                           => EmptyValue()

  private def accessSection: AccessSection => Value =
    _.member.pipe(member => function(in(_)(_ => member.$$)))

  private def accessApplySection: AccessApplySection => Value =
    _.member.pipe(member => function(in(_)(member.$$(_))))

  private def applySection: ApplySection => Value =
    _.node.$.pipe(body => function(_(body)))

  private def conditional(node: ShorthandConditional): Value =
    val left = node.condition.$

    if (left.isTruthy) left else node.alternative.$

  private def loop(node: ShorthandLoop, value: Option[Value] = None): Value =
    val left = node.condition.$

    if left.isTruthy
    then loop(node, Some(left))
    else
      value match
        case Some(value) => value
        case None        => node.alternative.$

  private def parentOf(node: ParentOf): Value =
    node.node.$.parent

  private def importExpression(node: Import): Value =
    s"$path/${node.names.mkString("/")}.meg"
      .pipe: fileName =>
        modules get fileName match
          case Some(module) => module
          case None =>
            FileReader.open(fileName):
              Cursor(_)
                .pipe(Lexer(_, path))
                .pipe(Parser.apply)
                .pipe(parser => (parser.parse(), parser.path))
            match
              case Failure(_)      => (Empty(), path)
              case Success(module) => module.tap(modules.update(fileName, _))
      .pipe(TreeWalker(_, modules, env).eval())

  private def identifierSectionLeft(node: IdentifierSectionLeft): Value =
    val operator = variable(false)(node.operator)

    function(left => function(operator(_)(left)))

  private def attributeDefine(node: AttributeDefine): Value =
    define(node.attribute, node.node.$, node.value.$)

  private def variableDefine(node: Define): Value =
    define(node.name, env, node.value.$)

  private def define: (String, Value, Value) => Value =
    operateAttributes(_.remove, _.define)

  private def operateAttributes(
      ifEmpty: Value => String => Value,
      ifNotEmpty: Value => Value => String => Value
  )(name: String, environment: Value, value: Value): Value =
    value.tap: _ =>
      if name == ""
      then environment.setBody(value.body)
      else
        value match
          case EmptyValue(true) => ifEmpty(environment)(name)
          case _                => ifNotEmpty(environment)(value)(name)

  private def lambda(node: Lambda): Value =
    AnyValue(env).tap: func =>
      func.setBody: argument =>
        in(AnyValue(func)):
          _.define(argument)(node.argument).pipe(_ => node.body.$)

  private def struct(node: Struct): Value =
    in(AnyValue(env))(_.tap(_ => node.nodes.foreach(_.$)))

  private def inherit(node: Inherit): Value =
    node.parent.$.pipe(left => node.child.$.tap(_.parent = left))

  private def conditional(node: Conditional): Value =
    if node.condition.$.isTruthy
    then node.consequence.$
    else node.alternative.$

  private def accessApply(node: AccessApply): Value =
    in(node.node.$)(node.member.$$(_))

  private def access(node: Access): Value =
    in(node.node.$)(_ => node.member.$$)

  private def applyExpression(node: Apply): Value =
    node.left.$(node.right.$)

  private def list(node: Sequence): Value =
    ListValue(ArrayBuffer from node.nodes.map(_.$))

  private def statements(node: Statements): Value =
    if node.nodes.length == 0
    then EmptyValue()
    else node.nodes.map(_.$).last

  private def variable(accessFromParent: Boolean): String => Value =
    env.get(if accessFromParent then _.parent else _.outer)

  private def loop(node: Loop, value: Option[Value]): Value =
    if node.condition.$.isTruthy
    then loop(node, Some(node.consequence.$))
    else
      value match
        case Some(value) => value
        case None        => node.alternative.$

  private def merge(node: Merge): Value =
    node.left.$.tap(_.attributes ++= node.right.$.attributes)

  private def env: Value =
    environments.top

  private def in[T](environment: Value)(expression: Value => T): T =
    environments
      .push(environment)
      .pipe(_ => expression(environment))
      .tap(_ => environments.pop())

  private inline implicit def toEvaluable(node: Node): Evaluable =
    Evaluable(node)

  private final class Evaluable(private val self: Node) extends Node:
    def $ : Value =
      eval(false)(self)

    def $$ : Value =
      eval(true)(self)
