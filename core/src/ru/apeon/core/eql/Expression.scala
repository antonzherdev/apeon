package ru.apeon.core.eql

import java.util.Date
import java.lang.String
import ru.apeon.core.entity._

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
}

case class ConstNull() extends Expression {
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

class Ref(val from : Option[String] = None, val column : String) extends Expression with From {
  var fromRef : From = null
  var columnRef : Field = null
  var isRefSet : Boolean = false

  override def toString = from match {
    case Some(a) => "%s.%s".format(a, column)
    case None => column
  }

  override def fillRef(env: Environment) {
    if(fromRef == null) {
      fromRef = from match {
        case Some(a) => env.fromOption(a).getOrElse(Ref(a))
        case None => env.from
      }
    }
    fromRef.fillRef(env)
    columnRef = fromRef.column(column)
    isRefSet = true
  }

  override def equals(obj: Any) = obj match {
    case ref : Ref => ref.from == from && ref.column == column
  }

  def name = column

  def columns = columnRef match {
    case o : ToOne => o.entity.fields
    case m : ToMany => m.entity.fields
  }

  def columnOption(name: String) = columnRef match {
    case o : ToOne => o.entity.fieldOption(name)
    case m : ToMany => m.entity.fieldOption(name)
  }

  def dataSource = null
}

object Ref {
  def apply(from : Option[String], column : String) : Ref = new Ref(from, column)
  def apply(column : String): Ref = new Ref(None, column)
  def apply(from : String, column : String) : Ref = new Ref(Some(from), column)

  def apply(from : From, column : Field) : Ref = {
    val r = new Ref(Some(from.name), column.name)
    r.fromRef = from
    r.columnRef = column
    r.isRefSet = true
    r
  }

  def apply(from : From, columnName : String) : Ref = {
    if(from.isRefSet)
      Ref(from, from.column(columnName))
    else {
      val r = Ref(from.name, columnName)
      r.fromRef = from
      r
    }
  }

  def unapply(ref : Ref) : Option[(Option[String], String)] = Some(ref.from, ref.column)
}

case class ConstNumeric(value : BigDecimal) extends Expression {
  override def toString = value.toString()
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
    case _ => new ConstString(v.toString)
  }
}

case class ConstString(value : String) extends Expression {
  override def toString = "\"%s\"".format(value)
}

case class ConstDate(value : Date) extends Expression

case class Parameter(name : String) extends Expression

abstract class FunctionCall extends Expression{
  val name : String
}

case class SqlFunctionCall(name : String, parameters : Seq[Expression]) extends FunctionCall {
  override def children = parameters

  override def build(children: Seq[Expression]) = SqlFunctionCall(name, children)
}

abstract class AggFunctionCall extends FunctionCall

abstract class AggPar1FunctionCall extends AggFunctionCall {
  def parameter : Expression

  override def children = Seq(parameter)


  override def build(children: Seq[Expression]) = build(children.head)

  def build(parameter : Expression) : Expression

  override def equals(obj: Any) = obj match {
    case e : AggPar1FunctionCall =>
      (e.name == this.name) &&
              (e.parameter == parameter)
    case _ => false
  }
}

case class Sum(parameter : Expression) extends AggPar1FunctionCall{
  val name = "sum"

  def build(parameter: Expression) = Sum(parameter)
}

case class ESelect(select : Expression, from : From, where : Option[Expression] = None) extends Expression {
  override def fillRef(env: Environment) {
    env.push(from)
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
}

case class Not(expression : Expression) extends Expression{
  override def fillRef(env: Environment) {
    expression.fillRef(env)
  }

  override def children = Seq(expression)

  override def build(children: Seq[Expression]) = Not(children.head)
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
}

abstract class External extends Expression {
  def eqlExpression : Expression
}