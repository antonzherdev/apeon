package ru.apeon.core.sql

import java.util.Date

abstract class Expression {
  override def toString = DefaultSqlDialect.toString(this, Map())
  def toString(parameters : scala.collection.Map[String, Any]) = DefaultSqlDialect.toString(this, parameters)
}

object Expression {
  def constant(v : Any) : Expression = v match {
    case null => ConstNull()
    case i : Int => new ConstNumeric(i)
    case d : BigDecimal => new ConstNumeric(d)
    case s : String => new ConstString(s)
    case d : Date =>  new ConstDate(d)
    case e : Expression => e
    case _ => new ConstString(v.toString)
  }
}

case class ConstNull() extends Expression

abstract class BinaryExpression extends Expression {
  def left : Expression
  def right : Expression
  def alias : String
}

case class Equal(left : Expression, right : Expression) extends BinaryExpression {
  val alias = "="
}

case class NotEqual(left : Expression, right : Expression) extends BinaryExpression {
  val alias = "<>"
}

case class More(left : Expression, right : Expression) extends BinaryExpression {
  val alias = ">"
}
case class MoreOrEqual(left : Expression, right : Expression) extends BinaryExpression {
  val alias = ">="
}
case class Less(left : Expression, right : Expression) extends BinaryExpression {
  val alias = "<"
}
case class LessOrEqual(left : Expression, right : Expression) extends BinaryExpression {
  val alias = "<="
}


class And(val left : Expression, val right : Expression) extends BinaryExpression {
  val alias = "and"

  override def equals(obj: Any) = obj match {
    case And(l , r) => l == left && r == right
    case _ => false
  }
}

object And{
  def apply(left : Expression, right : Expression) : And = new And(left, right)

  def apply(left : Expression, right : Option[Expression]) : Expression = right match {
    case Some(r) => new And(left, r)
    case None => left
  }

  def apply(left : Option[Expression], right : Expression) : Expression = left match {
    case Some(l) => new And(l, right)
    case None => right
  }

  def apply(left : Option[Expression], right : Option[Expression]) : Option[Expression] = left match {
    case Some(l) => right match {
      case Some(r) => Some(new And(l, r))
      case None => Some(l)
    }
    case None => right match {
      case Some(r) => Some(r)
      case None => None
    }
  }

  def unapply(and : And) = Some((and.left, and.right))
}

case class Or(left : Expression, right : Expression) extends BinaryExpression {
  val alias = "or"
}
case class Like(left : Expression, right : Expression) extends BinaryExpression{
  val alias = "like"
}

case class Not(expression : Expression) extends Expression

case class Exists(from : From, where : Option[Expression]) extends Expression


class Ref(val from : Option[String], val column : String) extends Expression {
  override def equals(obj: Any) = obj match {
    case r : Ref => r.from == from && r.column == column
    case _ => false
  }
}

object Ref {
  def apply(from :  Option[String], column : String) = new Ref(from, column)
  def apply(column : String) = new Ref(None, column)
  def apply(from :  String, column : String) = new Ref(Some(from), column)
  def apply(from :  From, column : String) = new Ref(Some(from.name), column)
}

case class ConstNumeric(value : BigDecimal) extends Expression
case class ConstString(value : String) extends Expression
case class ConstDate(value : Date) extends Expression

case class Parameter(name : String) extends Expression {
  override def equals(obj: Any) = obj match {
    case e : Parameter => (e.name == this.name)
    case _ => false
  }
}

case class Call(name : String, parameters : Seq[Expression]) extends Expression

case class ESelect(select : Expression, from : From, where : Option[Expression]) extends Expression

case class Identity(table : SqlTable) extends Expression

case class DeclarationRef(name : String) extends Expression
