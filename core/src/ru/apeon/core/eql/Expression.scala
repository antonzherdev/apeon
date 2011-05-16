package ru.apeon.core.eql

import java.util.Date
import java.lang.String
import ru.apeon.core.entity._
import ru.apeon.core._
import script.{Imports}

abstract class Expression {
  def fillRef(env : Environment) {
    children.foreach(_.fillRef(env))
  }

  def foreach(f : (Expression) => Unit) {
    f(this)
    children.foreach(_.foreach(f))
  }

  def map(f : PartialFunction[Expression, Expression]) : Expression =
    if(f.isDefinedAt(this)) f(this) else build(children.map(_.map(f)))

  protected def children : Seq[Expression] = Seq()

  protected def build(children : Seq[Expression]) : Expression = this

  def dataType(env: script.Environment = new script.DefaultEnvironment) : script.ScriptDataType
}

case class ConstNull() extends Expression {
  def dataType(env: script.Environment) = script.ScriptDataTypeNull()
}

abstract class BinaryExpression extends Expression {
  def left : Expression
  def right : Expression
  def name : String

  override def children = Seq(left, right)

  override def build(children: Seq[Expression]) = build(children.head, children.tail.head)

  def build(left : Expression, right : Expression) : Expression

  override def toString = "%s %s %s".format(left.toString, name, right.toString)
}

abstract class BinaryBooleanExpression extends BinaryExpression {
  def dataType(env: script.Environment) = script.ScriptDataTypeBoolean()
}

case class Equal(left : Expression, right : Expression) extends BinaryBooleanExpression {
  val name = "="
  def build(left: Expression, right: Expression) = Equal(left, right)
}
case class NotEqual(left : Expression, right : Expression) extends BinaryBooleanExpression {
  val name = "!="
  def build(left: Expression, right: Expression) = NotEqual(left, right)
}
case class More(left : Expression, right : Expression) extends BinaryBooleanExpression {
  val name = ">"
  def build(left: Expression, right: Expression) = More(left, right)
}
case class MoreOrEqual(left : Expression, right : Expression) extends BinaryBooleanExpression {
  val name = ">="
  def build(left: Expression, right: Expression) = MoreOrEqual(left, right)
}
case class Less(left : Expression, right : Expression) extends BinaryBooleanExpression {
  val name = "<"
  def build(left: Expression, right: Expression) = Less(left, right)
}
case class LessOrEqual(left : Expression, right : Expression) extends BinaryBooleanExpression {
  val name = "<="
  def build(left: Expression, right: Expression) = LessOrEqual(left, right)
}
case class Like(left : Expression, right : Expression) extends BinaryBooleanExpression{
  val name = "like"
  def build(left: Expression, right: Expression) = Like(left, right)
}
case class And(left : Expression, right : Expression) extends BinaryBooleanExpression {
  val name = "and"
  def build(left: Expression, right: Expression) = And(left, right)
}
case class Or(left : Expression, right : Expression) extends BinaryBooleanExpression{
  val name = "or"
  def build(left: Expression, right: Expression) = Or(left, right)
}

object Dot{
  def apply(left : Expression, right : Ref) : Dot = new Dot(left, right)
  def apply(left : Expression, right : String) : Dot = new Dot(left, Ref(right))
  def apply(left : Expression, right : script.Declaration) : Dot = new Dot(left, Ref(right))

  def apply(left : String, right : Ref) : Dot = new Dot(Ref(left), right)
  def apply(left : String, right : String) : Dot = new Dot(Ref(left), Ref(right))
  def apply(left : String, right : script.Declaration) : Dot = new Dot(Ref(left), Ref(right))

  def apply(left : From, right : Ref) : Dot = new Dot(Ref(left), right)
  def apply(left : From, right : String) : Dot = new Dot(Ref(left), Ref(right))
  def apply(left : From, right : script.Declaration) : Dot = new Dot(Ref(left), Ref(right))

  def apply(parts : String) : Dot = {
    var p = parts.split('.').toSeq
    val first = p.head
    p = p.tail
    var ret = Dot(first, p.head)
    p = p.tail
    for(s <- p) {
      ret = Dot(ret, s)
    }
    ret
  }

  def unapply(dot : Dot) : Option[(Expression, Ref)] = Some((dot.left, dot.right))
}

class Dot(val left : Expression, val right : Ref) extends Expression {
  override def toString = "%s.%s".format(left, right)

  override def fillRef(env: Environment) {
    left.fillRef(env)
    env.withDot(this) {
      right.fillRef(env)
    }
  }

  override def equals(obj: Any) = obj match {
    case ref : Dot => ref.left == left && ref.right == right
    case _ => false
  }

  def dataType(env: script.Environment) = right.dataType(env)

  override protected def children = Seq(left, right)
}

object Ref {
  def apply(name : String) : Ref = new Ref(name)
  def apply(from : From) : Ref = new Ref(from)
  def apply(d : script.Declaration) : Ref = new Ref(d)
  def apply(name : String, parameters : Expression*) : Ref = new Ref(name, parameters.toSeq)

  def unapply(r : Ref) : Option[String] = Some(r.name)
}

class Ref(val name : String, val parameters : Seq[Expression] = Seq()) extends Expression{
  abstract class RefData {
    def from : From = throw new RuntimeException("No from")
    def declaration : script.Declaration = throw new RuntimeException("No declaration")
    def dataType(env: script.Environment) : script.ScriptDataType
  }
  case class NoData() extends RefData{
    def dataType(env: script.Environment) =  throw new RuntimeException("No data")
  }
  case class FromData(override val from : From) extends RefData {
    def dataType(env: script.Environment) = ScriptDataTypeFrom(from)
  }
  case class DeclarationData(override val declaration : script.Declaration) extends RefData {
    def dataType(env: script.Environment) = declaration.dataType(env, None)
  }
  protected[eql] var data : RefData = NoData()
  private var _defaultFrom : From = _

  def this(from : From) {
    this(from.name)
    data = FromData(from)
  }

  def this(dec : script.Declaration) {
    this(dec.name)
    data = DeclarationData(dec)
  }

  override def fillRef(env: Environment) {
    data = env.dot match {
      case None => env.fromOption(name) match {
        case None => DeclarationData(env.from.field(name))
        case Some(from) => FromData(from)
      }
      case Some(dot) => {
        val e = new script.DefaultEnvironment(env.model)
        val declaration = dot.left.dataType(e).declaration(e, name, parameters.map{
          par => script.Par(new script.Expression{
            def preFillRef(model: script.Environment, imports: Imports){}
            def fillRef(env: script.Environment, imports: Imports){}
            def evaluate(env: script.Environment) = null
            def dataType(env: script.Environment) = par.dataType(env)
          })
        } match  {
          case Seq() => None
          case s => Some(s)
        }).getOrElse{
          throw new RuntimeException("Function %s not found".format(name))
        }
        if(!declaration.isInstanceOf[SqlGeneration] && !declaration.isInstanceOf[Field]) {
          throw new RuntimeException("Function %s is not supported in eql".format(name))
        }
        DeclarationData(declaration)
      }
    }
    _defaultFrom = env.from
    parameters.foreach(_.fillRef(env))
  }

  def dataType(env: script.Environment) : script.ScriptDataType = data.dataType(env)

  def declaration = data.declaration
  def from = data.from

  def defaultFrom = _defaultFrom

  case class ScriptDataTypeFrom(from : From) extends script.ScriptDataType {
    override def declarations = from.fields
  }

  def isFrom = data.isInstanceOf[FromData]
  def isDeclaration = data.isInstanceOf[DeclarationData]
  def isToMany = isDeclaration && declaration.isInstanceOf[ToMany]
  def isToOne = isDeclaration && declaration.isInstanceOf[ToOne]

  override def equals(obj: Any) = obj match {
    case r : Ref => r.name == this.name
    case _ => false
  }

  override protected def children = parameters

  override def toString = name
}

case class ConstNumeric(value : BigDecimal) extends Expression {
  override def toString = value.toString()

  def dataType(env: script.Environment) = script.ScriptDataTypeDecimal()
}


object Const {
  def unapply(d : ConstNumeric) : Option[Double] = Some(d.value.toDouble)

  def apply(v : Any) : Expression = v match {
    case null => ConstNull()
    case i : Int => ConstNumeric(i)
    case d : BigDecimal => ConstNumeric(d)
    case s : String => ConstString(s)
    case d : Date =>  ConstDate(d)
    case e : Expression => e
    case e : Entity => e.id.const
    case c : ConstObject => c.expression
    case _ => new ConstString(v.toString)
  }
}

trait ConstObject {
  def expression : Expression
}

case class ConstString(value : String) extends Expression {
  override def toString = "\"%s\"".format(value)

  def dataType(env: script.Environment) = script.ScriptDataTypeString()
}

case class ConstDate(value : Date) extends Expression {
  def dataType(env: script.Environment) = script.ScriptDataTypeDate()
}

case class Parameter(name : String) extends Expression {
  def dataType(env: script.Environment) = script.ScriptDataTypeAny()
}

case class ESelect(select : Expression, from : From, where : Option[Expression] = None) extends Expression {
  override def fillRef(env: Environment) {
    env.push(from)
    from.fillRef(env)
    select.fillRef(env)
    where.foreach(_.fillRef(env))
    env.pop()
  }

  override def children = where match {
    case None => Seq(select)
    case _ => Seq(select, where.get)
  }

  override def build(children: Seq[Expression]) =
    ESelect(children.head, from, if(children.size == 2) Some(children.tail.head) else None)

  def dataType(env: script.Environment) = select.dataType(env)
}

case class Not(expression : Expression) extends Expression{
  override def fillRef(env: Environment) {
    expression.fillRef(env)
  }

  override def children = Seq(expression)

  override def build(children: Seq[Expression]) = Not(children.head)

  def dataType(env: script.Environment) = script.ScriptDataTypeBoolean()
}

case class Exists(from : From, where : Option[Expression] = None) extends Expression {
  override def fillRef(env: Environment) {
    from.fillRef(env)
    env.push(from)
    where.foreach(_.fillRef(env))
    env.pop()
  }

  override def children = where match {
    case None => Seq()
    case _ => Seq(where.get)
  }

  override def build(children: Seq[Expression]) = children match {
    case Seq() => Exists(from)
    case Seq(where) => Exists(from, Some(where))
  }

  def dataType(env: script.Environment) = script.ScriptDataTypeBoolean()
}

abstract class External extends Expression {
  def eqlExpression : Expression
}