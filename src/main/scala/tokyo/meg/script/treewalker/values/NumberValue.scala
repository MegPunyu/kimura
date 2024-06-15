package tokyo.meg.script.treewalker.values

object NumberValue extends HasSingletonRoot[NumberValue]:
  val singletonRoot = NumberValue()

  initialize()

final private case class NumberValue() extends Value:
  import Value._

  private val I = IntValue
  private val R = RealValue
  private val S = StringValue

  attributes.addAll(
    Array(
      "+" -> add,
      "-" -> sub,
      "*" -> mul,
      "/" -> div,
      "%" -> mod,
      "**" -> pow,
      "<" -> lt,
      ">" -> gt,
      "<=" -> le,
      ">=" -> ge,
      "==" -> _eq,
      "!=" -> neq,
      "&&" -> and,
      "||" -> or,
      "^^" -> xor,
      "!&" -> nand,
      "!|" -> nor,
      "!^" -> xnor,
      "&" -> bitAnd,
      "|" -> bitOr,
      "^" -> bitXor,
      "~&" -> bitNand,
      "~|" -> bitNor,
      "~^" -> bitXnor,
      ">>>" -> unsignedRightShift,
      ">>" -> rightShift,
      "<<" -> leftShift,
      "--" -> neg,
      "!" -> not,
      "~" -> inv,
      "abs" -> abs,
      "sign" -> sign,
      "floor" -> floor,
      "ceil" -> ceil,
      "round" -> round,
      "max" -> max,
      "min" -> min,
      "exp" -> exp,
      "log" -> log,
      "log10" -> log10,
      "log2" -> log2,
      "logE" -> logE,
      "sin" -> sin,
      "cos" -> cos,
      "tan" -> tan,
      "asin" -> asin,
      "acos" -> acos,
      "atan" -> atan,
      "atan2" -> atan2,
      "sinh" -> sinh,
      "cosh" -> cosh,
      "tanh" -> tanh
    )
  )

  def add: Value = biFunction:
    case (I(a), I(b)) => a + b
    case (I(a), R(b)) => a + b
    case (R(a), I(b)) => a + b
    case (R(a), R(b)) => a + b
    case (I(a), b)    => a + b._toString
    case (R(a), b)    => a + b._toString
    case _            => EmptyValue()

  def sub: Value = biFunction:
    case (I(a), I(b)) => a - b
    case (I(a), R(b)) => a - b
    case (R(a), I(b)) => a - b
    case (R(a), R(b)) => a - b
    case _            => EmptyValue()

  def mul: Value = biFunction:
    case (ref @ I(0), I(b)) => ref
    case (ref @ R(0), R(b)) => ref
    case (I(a), I(b))       => a * b
    case (I(a), R(b))       => a * b
    case (R(a), I(b))       => a * b
    case (R(a), R(b))       => a * b
    case _                  => EmptyValue()

  def div: Value = biFunction:
    case (I(a), I(b)) => a.toDouble / b
    case (I(a), R(b)) => a / b
    case (R(a), I(b)) => a / b
    case (R(a), R(b)) => a / b
    case _            => EmptyValue()

  def mod: Value = biFunction:
    case (I(a), I(b)) => a % b
    case (I(a), R(b)) => a % b
    case (R(a), I(b)) => a % b
    case (R(a), R(b)) => a % b
    case _            => EmptyValue()

  def pow: Value = biFunction:
    case (I(a), I(b)) => Math.pow(a, b)
    case (I(a), R(b)) => Math.pow(a, b)
    case (R(a), I(b)) => Math.pow(a, b)
    case (R(a), R(b)) => Math.pow(a, b)
    case _            => EmptyValue()

  def lt: Value = biFunction:
    case (I(a), I(b)) => a < b
    case (I(a), R(b)) => a < b
    case (R(a), I(b)) => a < b
    case (R(a), R(b)) => a < b
    case _            => EmptyValue()

  def gt: Value = biFunction:
    case (I(a), I(b)) => a > b
    case (I(a), R(b)) => a > b
    case (R(a), I(b)) => a > b
    case (R(a), R(b)) => a > b
    case _            => EmptyValue()

  def le: Value = biFunction:
    case (I(a), I(b)) => a <= b
    case (I(a), R(b)) => a <= b
    case (R(a), I(b)) => a <= b
    case (R(a), R(b)) => a <= b
    case _            => EmptyValue()

  def ge: Value = biFunction:
    case (I(a), I(b)) => a >= b
    case (I(a), R(b)) => a >= b
    case (R(a), I(b)) => a >= b
    case (R(a), R(b)) => a >= b
    case _            => EmptyValue()

  def _eq: Value = biFunction:
    case (I(a), I(b)) => a == b
    case (I(a), R(b)) => a == b
    case (R(a), I(b)) => a == b
    case (R(a), R(b)) => a == b
    case (I(_), _)    => false
    case (R(_), _)    => false
    case _            => EmptyValue()

  def neq: Value = biFunction:
    case (I(a), I(b)) => a != b
    case (I(a), R(b)) => a != b
    case (R(a), I(b)) => a != b
    case (R(a), R(b)) => a != b
    case (I(_), _)    => true
    case (R(_), _)    => true
    case _            => EmptyValue()

  def and: Value = biFunction:
    case (I(a), I(b)) => a * b != 0
    case (I(a), R(b)) => a * b != 0
    case (R(a), I(b)) => a * b != 0
    case (R(a), R(b)) => a * b != 0
    case _            => EmptyValue()

  def or: Value = biFunction:
    case (I(a), I(b)) => a != 0 || b != 0
    case (I(a), R(b)) => a != 0 || b != 0
    case (R(a), I(b)) => a != 0 || b != 0
    case (R(a), R(b)) => a != 0 || b != 0
    case _            => EmptyValue()

  def xor: Value = biFunction:
    case (I(a), I(b)) => a == 0 && b != 0 || a != 0 && b == 0
    case (I(a), R(b)) => a == 0 && b != 0 || a != 0 && b == 0
    case (R(a), I(b)) => a == 0 && b != 0 || a != 0 && b == 0
    case (R(a), R(b)) => a == 0 && b != 0 || a != 0 && b == 0
    case _            => EmptyValue()

  def nand: Value = biFunction:
    case (I(a), I(b)) => a * b == 0
    case (I(a), R(b)) => a * b == 0
    case (R(a), I(b)) => a * b == 0
    case (R(a), R(b)) => a * b == 0
    case _            => EmptyValue()

  def nor: Value = biFunction:
    case (I(a), I(b)) => a == 0 && b == 0
    case (I(a), R(b)) => a == 0 && b == 0
    case (R(a), I(b)) => a == 0 && b == 0
    case (R(a), R(b)) => a == 0 && b == 0
    case _            => EmptyValue()

  def xnor: Value = biFunction:
    case (I(a), I(b)) => !(a == 0 && b != 0 || a != 0 && b == 0)
    case (I(a), R(b)) => !(a == 0 && b != 0 || a != 0 && b == 0)
    case (R(a), I(b)) => !(a == 0 && b != 0 || a != 0 && b == 0)
    case (R(a), R(b)) => !(a == 0 && b != 0 || a != 0 && b == 0)
    case _            => EmptyValue()

  def bitAnd: Value = biFunction:
    case (I(a), I(b)) => a & b
    case (I(a), R(b)) => a & b.toLong
    case (R(a), I(b)) => a.toLong & b
    case (R(a), R(b)) => a.toLong & b.toLong
    case _            => EmptyValue()

  def bitOr: Value = biFunction:
    case (I(a), I(b)) => a | b
    case (I(a), R(b)) => a | b.toLong
    case (R(a), I(b)) => a.toLong | b
    case (R(a), R(b)) => a.toLong | b.toLong
    case _            => EmptyValue()

  def bitXor: Value = biFunction:
    case (I(a), I(b)) => a ^ b
    case (I(a), R(b)) => a ^ b.toLong
    case (R(a), I(b)) => a.toLong ^ b
    case (R(a), R(b)) => a.toLong ^ b.toLong
    case _            => EmptyValue()

  def bitNand: Value = biFunction:
    case (I(a), I(b)) => ~(a & b)
    case (I(a), R(b)) => ~(a & b.toLong)
    case (R(a), I(b)) => ~(a.toLong & b)
    case (R(a), R(b)) => ~(a.toLong & b.toLong)
    case _            => EmptyValue()

  def bitNor: Value = biFunction:
    case (I(a), I(b)) => ~(a | b)
    case (I(a), R(b)) => ~(a | b.toLong)
    case (R(a), I(b)) => ~(a.toLong | b)
    case (R(a), R(b)) => ~(a.toLong | b.toLong)
    case _            => EmptyValue()

  def bitXnor: Value = biFunction:
    case (I(a), I(b)) => ~(a ^ b)
    case (I(a), R(b)) => ~(a ^ b.toLong)
    case (R(a), I(b)) => ~(a.toLong ^ b)
    case (R(a), R(b)) => ~(a.toLong ^ b.toLong)
    case _            => EmptyValue()

  def unsignedRightShift: Value = biFunction:
    case (I(a), I(b)) => a >>> b
    case (I(a), R(b)) => a >>> b.toLong
    case (R(a), I(b)) => a.toLong >>> b
    case (R(a), R(b)) => a.toLong >>> b.toLong
    case _            => EmptyValue()

  def rightShift: Value = biFunction:
    case (I(a), I(b)) => a >> b
    case (I(a), R(b)) => a >> b.toLong
    case (R(a), I(b)) => a.toLong >> b
    case (R(a), R(b)) => a.toLong >> b.toLong
    case _            => EmptyValue()

  def leftShift: Value = biFunction:
    case (I(a), I(b)) => a << b
    case (I(a), R(b)) => a << b.toLong
    case (R(a), I(b)) => a.toLong << b
    case (R(a), R(b)) => a.toLong << b.toLong
    case _            => EmptyValue()

  def neg: Value = function:
    case I(x) => -x
    case R(x) => -x
    case _    => EmptyValue()

  def not: Value = function:
    case I(x) => x == 0
    case R(x) => x == 0
    case _    => EmptyValue()

  def inv: Value = function:
    case I(x) => ~x
    case R(x) => ~x.toLong
    case _    => EmptyValue()

  def abs: Value = function:
    case I(x) => Math.abs(x)
    case R(x) => Math.abs(x)
    case _    => EmptyValue()

  def sign: Value = function:
    case I(x) => Math.signum(x)
    case R(x) => Math.signum(x)
    case _    => EmptyValue()

  def floor: Value = function:
    case I(x) => Math.floor(x)
    case R(x) => Math.floor(x)
    case _    => EmptyValue()

  def ceil: Value = function:
    case I(x) => Math.ceil(x)
    case R(x) => Math.ceil(x)
    case _    => EmptyValue()

  def round: Value = function:
    case I(x) => Math.round(x)
    case R(x) => Math.round(x)
    case _    => EmptyValue()

  def max: Value = biFunction:
    case (I(a), I(b)) => Math.max(a, b)
    case (I(a), R(b)) => Math.max(a, b)
    case (R(a), I(b)) => Math.max(a, b)
    case (R(a), R(b)) => Math.max(a, b)
    case _            => EmptyValue()

  def min: Value = biFunction:
    case (I(a), I(b)) => Math.min(a, b)
    case (I(a), R(b)) => Math.min(a, b)
    case (R(a), I(b)) => Math.min(a, b)
    case (R(a), R(b)) => Math.min(a, b)
    case _            => EmptyValue()

  def exp: Value = function:
    case I(x) => Math.exp(x)
    case R(x) => Math.exp(x)
    case _    => EmptyValue()

  def log: Value = biFunction:
    case (I(base), I(x)) => Math.log(x) / Math.log(base)
    case (I(base), R(x)) => Math.log(x) / Math.log(base)
    case (R(base), I(x)) => Math.log(x) / Math.log(base)
    case (R(base), R(x)) => Math.log(x) / Math.log(base)
    case _               => EmptyValue()

  def log10: Value = function:
    case I(x) => Math.log10(x)
    case R(x) => Math.log10(x)
    case _    => EmptyValue()

  def log2: Value = function:
    case I(x) => Math.log(x) / Math.log(2)
    case R(x) => Math.log(x) / Math.log(2)
    case _    => EmptyValue()

  def logE: Value = function:
    case I(x) => Math.log(x)
    case R(x) => Math.log(x)
    case _    => EmptyValue()

  def sin: Value = function:
    case I(x) => Math.sin(x)
    case R(x) => Math.sin(x)
    case _    => EmptyValue()

  def cos: Value = function:
    case I(x) => Math.cos(x)
    case R(x) => Math.cos(x)
    case _    => EmptyValue()

  def tan: Value = function:
    case I(x) => Math.tan(x)
    case R(x) => Math.tan(x)
    case _    => EmptyValue()

  def asin: Value = function:
    case I(x) => Math.asin(x)
    case R(x) => Math.asin(x)
    case _    => EmptyValue()

  def acos: Value = function:
    case I(x) => Math.acos(x)
    case R(x) => Math.acos(x)
    case _    => EmptyValue()

  def atan: Value = function:
    case I(x) => Math.atan(x)
    case R(x) => Math.atan(x)
    case _    => EmptyValue()

  def atan2: Value = biFunction:
    case (I(a), I(b)) => Math.atan2(a, b)
    case (I(a), R(b)) => Math.atan2(a, b)
    case (R(a), I(b)) => Math.atan2(a, b)
    case (R(a), R(b)) => Math.atan2(a, b)
    case _            => EmptyValue()

  def sinh: Value = function:
    case I(x) => Math.sinh(x)
    case R(x) => Math.sinh(x)
    case _    => EmptyValue()

  def cosh: Value = function:
    case I(x) => Math.cosh(x)
    case R(x) => Math.cosh(x)
    case _    => EmptyValue()

  def tanh: Value = function:
    case I(x) => Math.tanh(x)
    case R(x) => Math.tanh(x)
    case _    => EmptyValue()
