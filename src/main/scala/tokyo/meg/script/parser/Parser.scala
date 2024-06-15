package tokyo.meg.script.parser

import scala.collection.mutable._
import scala.util.chaining._

import tokyo.meg.script.util._
import tokyo.meg.script.lexer._
import tokyo.meg.script.parser.ast._

class Parser(val lexer: Lexer):
  import TokenCategory._
  import DefaultReservedTokens._
  import lexer._
  import reservedTokens._

  private var (currentToken, nextToken): (Token, Token) = (null, null)

  this()()

  inline def path: String =
    lexer.path

  inline def parse(): Node =
    level0()

  private def level0(): Node =
    nodeSequence(level1, Semi)() pipe statements

  private def level1(): Node =
    val token = if (tokens(WHERE) == DEFAULT_WHERE) Colon else Where

    nodeSequence(() => level2(), token)().reverse pipe statements

  @annotation.tailrec
  private def level2(node: Node = level3()): Node =
    currentToken.category match
      case GtLt => this().level2(Merge(node, level3()))
      case _    => node

  @annotation.tailrec
  private def level3(node: Node = level4()): Node =
    currentToken.category match
      case LFunnel => this().level3(Apply(node, level4()))
      case RFunnel => this().level3(Apply(level4(), node))
      case _       => node

  private def level4(): Node =
    val ConditionalToken = if (tokens(IF) == DEFAULT_IF) Quest else If

    level5().pipe:
      currentToken.category match
        case ConditionalToken => ternary(Conditional.apply)
        case At               => ternary(Loop.apply)
        case QuestColon       => ShorthandConditional(_, this().level4())
        case AtColon          => ShorthandLoop(_, this().level4())
        case LArrow           => Inherit(_, this().level4())
        case _                => identity

  private def level5(node: Node = level6()): Node =
    currentToken.category match
      case Operator           => operatorInfix(node, currentToken.value)
      case IdentifierOperator => identifierInfix(node, currentToken.value)
      case _                  => node

  private def level6(node: Node = level7()): Node =
    currentToken.category match
      case Period       => this().accessApply(node)
      case ColonColon   => this().accessOrAttributeDefine(node)
      case PeriodPeriod => level6(this(ParentOf(node)))
      case _            => applyOrLevel7(node)

  private def level7(): Node =
    val AnonymousToken =
      if (tokens(PLACEHOLDER) == DEFAULT_PLACEHOLDER) Quest else Placeholder

    val value = currentToken.value

    currentToken.category match
      case Dollar                => importExpression()
      case Identifier | Operator => startWithDeclarable(value)
      case IdentifierOperator    => this(IdentifierSectionLeft(value))
      case Anonymous             => this(Variable(value))
      case Equals                => Define("", this().level2())
      case RArrow                => ApplySection(this().level4())
      case Indent                => Statements(nodesUntil(Unindent))
      case LParen                => emptyOrGroupedExpression()
      case LBrack                => Sequence(nodesUntil(RBrack))
      case LBrace                => Struct(nodesUntil(RBrace))
      case RParen                => this(Empty())
      case RBrack                => this(Sequence(Array()))
      case RBrace                => this(Struct(Array()))
      case PeriodPeriod          => this(Parent())
      case ColonColon            => outerOrAccessSection()
      case Period                => thisOrAccessApplySection()
      case AnonymousToken        => anonymous()
      case LiteralStr            => this(ConstString(value))
      case LiteralInt            => this(ConstInt(value.toLong))
      case LiteralReal           => this(ConstReal(value.toDouble))
      case _                     => Empty()

  private def anonymous(): Node =
    currentToken = Token(currentToken.line, currentToken.column)(
      currentToken.value,
      Anonymous
    )

    Lambda(currentToken.value, level4())

  private def outerOrAccessSection(): Node =
    this().memberSections(AccessSection(_), Outer.apply)

  private def thisOrAccessApplySection(): Node =
    this().memberSections(AccessApplySection(_), This.apply)

  private def memberSections(section: Node => Node, other: () => Node): Node =
    if currentToken.is(Operator) || currentToken.isAppliable
    then section(level7())
    else other()

  private def apply[T](value: T = this): T =
    currentToken = nextToken

    getToken().tap: token =>
      if token.is(Comment) || token.is(Macro)
      then this()
      else nextToken = token

    value

  @annotation.tailrec
  private def nodeSequence(parser: () => Node, separator: TokenCategory)(
      nodes: ArrayDeque[Node] = ArrayDeque(parser())
  ): Seq[Node] =
    if currentToken is separator then
      if nextToken.isParenthesisEnd
      then this(nodes append Empty())
      else this().nodeSequence(parser, separator)(nodes append parser())
    else nodes

  private def ternary(constructor: (Node, Node, Node) => Node): Node => Node =
    val right = this().level4()
    val elseToken = if (tokens(ELSE) == DEFAULT_ELSE) Colon else Else

    if currentToken.category == elseToken
    then this(constructor(_, right, level4()))
    else constructor(_, right, Empty())

  private def statements(nodes: Seq[Node]): Node =
    if nodes.length == 1
    then nodes(0)
    else Statements(nodes.toArray)

  private def operatorInfix(node: Node, operator: String): Node =
    infixExpression(Apply(AccessApply(node, Variable(operator)), _))

  private def identifierInfix(node: Node, operator: String): Node =
    infixExpression(Apply(Apply(Variable(operator), node), _))

  private def infixExpression(f: Node => Node): Node =
    this().level5.compose(f):
      if currentToken.is(Operator) || currentToken.isAppliable
      then level6()
      else Empty()

  private def accessApply(node: Node): Node =
    level6(AccessApply(node, level7()))

  private def applyOrLevel7(node: Node): Node =
    if currentToken.isAppliable
    then level6(Apply(node, level7()))
    else node

  private def accessOrAttributeDefine(node: Node): Node =
    if currentToken.isDeclarable && nextToken.is(Equals)
    then AttributeDefine(node, currentToken.value, this()().level2())
    else
      currentToken.category match
        case Equals => AttributeDefine(node, "", this().level2())
        case _      => level6(Access(node, level7()))

  private def importExpression(names: ArrayDeque[String] = ArrayDeque()): Node =
    this()

    if currentToken is Identifier
    then importExpression(names append currentToken.value)
    else Import(names.toArray)

  private def startWithDeclarable: String => Node =
    nextToken.category match
      case Equals => Define(_, this()().level2())
      case RArrow => Lambda(_, this()().level4())
      case _      => unaryOrVariable()

  private def emptyOrGroupedExpression(): Node =
    if nextToken.category == RParen
    then this()(Empty(false))
    else Statements(nodesUntil(RParen))

  private def unaryOrVariable(): String => Node =
    if currentToken.is(Operator) && nextToken.isAppliable
    then Variable.apply.andThen(AccessApply(this().level7(), _))
    else this(Variable(_))

  private def nodesUntil(until: TokenCategory): Array[Node] =
    this()

    if currentToken is until
    then this(Array[Node]())
    else
      nodeSequence(level1, Semi)().toArray.tap: _ =>
        if currentToken is until
        then this()
