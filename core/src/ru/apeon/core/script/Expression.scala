package ru.apeon.core.script

trait Expression extends Statement {
  def dataType(env : Environment) : ScriptDataType

  def evaluate(env : Environment) : Any
}

abstract class BinaryOperator extends Expression {
  def left : Expression
  def right : Expression
  def dataType(env: Environment) = left.dataType(env)

  def fillRef(env : Environment, imports : Imports) {
    left.fillRef(env, imports)
    right.fillRef(env, imports)
  }

  def preFillRef(model: ObjectModel, imports: Imports) {
    left.preFillRef(model, imports)
    right.preFillRef(model, imports)
  }

  def name : String

  override def toString = "%s %s %s".format(left, name, right)
}

class Plus(val left : Expression, val right : Expression) extends BinaryOperator {
  def evaluate(env: Environment) = Plus.evaluate(env, left.evaluate(env), right.evaluate(env))

  val name = "+"

  override def equals(obj: Any) = obj match {
    case s : Plus => s.left == left && s.right == right
    case _ => false
  }
}

object Plus {
  def apply(left : Expression, right : Expression) = new Plus(left, right)

  def evaluate(env: Environment, left : Any, right : Any) : Any = left match {
    case i : Int => right match {
        case j : Int => i + j
        case s : String => i + s.toInt
        case d : BigDecimal => d + i
        case u : Object => throw ScriptException(env, "Unsupported datatype \"%s\" for plus with Int".format(u.getClass))
        case _ => throw ScriptException(env, "Unsupported datatype for plus with Int")
      }
    case d : BigDecimal => right match {
      case j : Int => d + j
      case s : String => d + s.toDouble
      case j : BigDecimal => d + j
      case u : Object => throw ScriptException(env, "Unsupported datatype \"%s\" for plus with Decimal".format(u.getClass))
      case _ => throw ScriptException(env, "Unsupported datatype for plus with Decimal")
    }
    case s : String => s + right.toString
    case l : Traversable[Any] => right match {
      case r : Traversable[Any] => l ++ r
      case r : Any => l ++ Seq(r)
      case _ => throw ScriptException(env, "Unsupported datatype for plus with Seq")
    }
    case u : Object => throw ScriptException(env, "Unsupported datatype \"%s\" for plus.".format(u.getClass))
    case _ => throw ScriptException(env, "Unsupported datatype for plus")
  }
}

class Minus(val left : Expression, val right : Expression) extends BinaryOperator {
  val name = "-"

  def evaluate(env: Environment) = Minus.evaluate(env, left.evaluate(env), right.evaluate(env))

  override def equals(obj: Any) = obj match {
    case s : Minus => s.left == left && s.right == right
    case _ => false
  }
}

object Minus {
  def apply(left : Expression, right : Expression) = new Minus(left, right)

  def evaluate(env: Environment, left : Any, right : Any) : Any = left match {
    case i : Int => right match {
        case j : Int => i - j
        case d : BigDecimal => BigDecimal(i) - d
        case u : Object => throw ScriptException(env, "Unsupported datatype \"%s\" for minus with Int".format(u.getClass))
        case _ => throw ScriptException(env, "Unsupported datatype for minus with Int")
      }
    case d : BigDecimal => right match {
      case j : Int => d - j
      case s : String => d - BigDecimal(s.toDouble)
      case j : BigDecimal => d - j
      case u : Object => throw ScriptException(env, "Unsupported datatype \"%s\" for minus with Decimal".format(u.getClass))
      case _ => throw ScriptException(env, "Unsupported datatype for minus with Decimal")
    }
    case u : Object => throw ScriptException(env, "Unsupported datatype \"%s\" for minus.".format(u.getClass))
    case _ => throw ScriptException(env, "Unsupported datatype for minus")
  }
}

class Mul(val left : Expression, val right : Expression) extends BinaryOperator {
  val name = "*"

  def evaluate(env: Environment) = Mul.evaluate(env, left.evaluate(env), right.evaluate(env))

  override def equals(obj: Any) = obj match {
    case s : Mul => s.left == left && s.right == right
    case _ => false
  }
}

object Mul {
  def apply(left : Expression, right : Expression) = new Mul(left, right)

  def evaluate(env: Environment, left : Any, right : Any) : Any = left match {
    case i : Int => right match {
        case j : Int => i*j
        case d : BigDecimal => BigDecimal(i)*d
        case u : Object => throw ScriptException(env, "Unsupported datatype \"%s\" for mul with Int".format(u.getClass))
        case _ => throw ScriptException(env, "Unsupported datatype for mul with Int")
      }
    case d : BigDecimal => right match {
      case j : Int => d*j
      case s : String => d*BigDecimal(s.toDouble)
      case j : BigDecimal => d*j
      case u : Object => throw ScriptException(env, "Unsupported datatype \"%s\" for mul with Decimal".format(u.getClass))
      case _ => throw ScriptException(env, "Unsupported datatype for mul with Decimal")
    }
    case u : Object => throw ScriptException(env, "Unsupported datatype \"%s\" for mul.".format(u.getClass))
    case _ => throw ScriptException(env, "Unsupported datatype for mul")
  }
}


class Div(val left : Expression, val right : Expression) extends BinaryOperator {
  val name = "/"

  def evaluate(env: Environment) = Div.evaluate(env, left.evaluate(env), right.evaluate(env))

  override def equals(obj: Any) = obj match {
    case s : Div => s.left == left && s.right == right
    case _ => false
  }
}

object Div {
  def apply(left : Expression, right : Expression) = new Div(left, right)

  def evaluate(env: Environment, left : Any, right : Any) : Any = left match {
    case i : Int => right match {
        case j : Int => i/j
        case d : BigDecimal => BigDecimal(i)/d
        case u : Object => throw ScriptException(env, "Unsupported datatype \"%s\" for div with Int".format(u.getClass))
        case _ => throw ScriptException(env, "Unsupported datatype for div with Int")
      }
    case d : BigDecimal => right match {
      case j : Int => d/j
      case s : String => d/BigDecimal(s.toDouble)
      case j : BigDecimal => d/j
      case u : Object => throw ScriptException(env, "Unsupported datatype \"%s\" for div with Decimal".format(u.getClass))
      case _ => throw ScriptException(env, "Unsupported datatype for div with Decimal")
    }
    case u : Object => throw ScriptException(env, "Unsupported datatype \"%s\" for div.".format(u.getClass))
    case _ => throw ScriptException(env, "Unsupported datatype for div")
  }
}

abstract class BooleanBinaryExpression extends BinaryOperator{
  override def dataType(env: Environment) = ScriptDataTypeBoolean()
}

case class Equal(left : Expression, right : Expression) extends BooleanBinaryExpression {
  val name = "=="

  def evaluate(env: Environment) = left.evaluate(env) == right.evaluate(env)
}

case class NotEqual(left : Expression, right : Expression) extends BooleanBinaryExpression {
  val name = "!="

  def evaluate(env: Environment) = left.evaluate(env) != right.evaluate(env)
}

abstract class CompareOperator extends BooleanBinaryExpression {
  def evaluate(env: Environment) = (left.evaluate(env), right.evaluate(env)) match {
    case (i: Int, j : Int) => evaluate(i, j)
    case (i : BigDecimal, j: Int) => evaluate(i, BigDecimal(j))
    case (i : Int, j: BigDecimal) => evaluate(BigDecimal(i), j)
    case (i : BigDecimal, j: BigDecimal) => evaluate(i, j)
  }
  def evaluate(left : Int, right : Int) : Boolean
  def evaluate(left : BigDecimal, right : BigDecimal) : Boolean
}

case class More(left : Expression, right : Expression) extends CompareOperator {
  val name = ">"
  def evaluate(left: Int, right: Int) = left > right
  def evaluate(left: BigDecimal, right: BigDecimal) = left > right
}

case class Less(left : Expression, right : Expression) extends CompareOperator {
  val name = "<"
  def evaluate(left: Int, right: Int) = left < right
  def evaluate(left: BigDecimal, right: BigDecimal) = left < right
}

case class MoreOrEqual(left : Expression, right : Expression) extends CompareOperator {
  val name = ">="
  def evaluate(left: Int, right: Int) = left >= right
  def evaluate(left: BigDecimal, right: BigDecimal) = left >= right
}

case class LessOrEqual(left : Expression, right : Expression) extends CompareOperator {
  val name = "<="
  def evaluate(left: Int, right: Int) = left <= right
  def evaluate(left: BigDecimal, right: BigDecimal) = left <= right
}

case class And(left : Expression, right : Expression) extends BooleanBinaryExpression {
  val name = "&&"
  def evaluate(env: Environment) = left.evaluate(env).asInstanceOf[Boolean] && right.evaluate(env).asInstanceOf[Boolean]
}

case class Or(left : Expression, right : Expression) extends BooleanBinaryExpression {
  val name = "||"
  def evaluate(env: Environment) = left.evaluate(env).asInstanceOf[Boolean] || right.evaluate(env).asInstanceOf[Boolean]
}