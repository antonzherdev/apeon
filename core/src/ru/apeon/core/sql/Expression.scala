package ru.apeon.core.sql

import java.util.Date
import java.text.{SimpleDateFormat, DateFormat}

/**
 * @author Anton Zherdev
 */

abstract class Expression extends TextGen

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

case class ConstNull() extends Expression {
  def textGen {
    append("null")
  }
}

abstract class BinaryExpression extends Expression {
  def left : Expression
  def right : Expression
  def alias : String

  def appendE(exp : Expression) {
    append(exp)
  }

  def textGen {
    appendE(left)
    append(' ')
    append(alias)
    append(' ')
    appendE(right)
  }
}

case class Equal(left : Expression, right : Expression) extends BinaryExpression {
  override def alias : String = right match {
    case p : Parameter => param(p.name) match  {
      case Some(l : List[_]) => "in"
      case _ => "="
    }
    case n : ConstNull => "is"
    case _ => "="
  }
}

case class NotEqual(left : Expression, right : Expression) extends BinaryExpression {
  override def alias : String = right match {
    case p : Parameter => param(p.name) match  {
      case Some(l : List[_]) => "not in"
      case _ => "<>"
    }
    case n : ConstNull => "is not"
    case _ => "<>"
  }
}

case class More(left : Expression, right : Expression) extends BinaryExpression {
  override def alias : String = ">"
}
case class MoreOrEqual(left : Expression, right : Expression) extends BinaryExpression {
  override def alias : String = ">="
}
case class Less(left : Expression, right : Expression) extends BinaryExpression {
  override def alias : String = "<"
}
case class LessOrEqual(left : Expression, right : Expression) extends BinaryExpression {
  override def alias : String = "<="
}


class And(val left : Expression, val right : Expression) extends BinaryExpression {
  override def appendE(exp: Expression) {
    exp match {
      case or: Or => {
        append('(')
        append(or)
        append(')')
      }
      case _ => super.appendE(exp)
    }
  }

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

case class Not(expression : Expression) extends Expression {
  def textGen {
    append("not(")
    append(expression)
    append(")")
  }
}

case class Exists(from : From, where : Option[Expression]) extends Expression {
  def textGen {
    append("exists(select * from")
    indent{
      indent(line(append(from)))
      if(where.isDefined) {
        line{append("where")}
        indent{line{append(where.get)}}
      }
    }
    line{append(')')}
  }
}


class Ref(val from : Option[String], val column : String) extends Expression {
  def textGen {
    if(from.isDefined) {
      append('"')
      append(from.get)
      append('"')
      append('.')
    }
    append('"')
    append(column)
    append('"')
  }

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

case class ConstNumeric(value : BigDecimal) extends Expression {
  def textGen {
    ConstantTextGen.textGen(value, this)
  }
}

private object ConstantTextGen {
  var dateFormat : DateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")

  def textGen(v : Any, tg : TextGen) {
    v match {
      case s: String => {
        tg.append('\'')
        s.foreach {
          c: Char =>
            c match {
              case '\'' => {
                tg.append('\'')
                tg.append('\'')
              }
              case _ => tg.append(c)
            }
        }
        tg.append('\'')
      }
      case d: Date => {
        tg.append('\'')
        tg.append(dateFormat.format(d))
        tg.append('\'')
      }
      case _ => tg.append(v)
    }
  }
}

case class ConstString(value : String) extends Expression {
  def textGen {
    ConstantTextGen.textGen(value, this)
  }
}

case class ConstDate(value : Date) extends Expression {
  def textGen {
    ConstantTextGen.textGen(value, this)
  }
}

case class Parameter(name : String) extends Expression {
  override def equals(obj: Any) = obj match {
    case e : Parameter => (e.name == this.name)
    case _ => false
  }


  def textGen {
    param(name) match {
      case None => {
        append(':')
        append(name)
      }
      case Some(l: List[_]) => {
        append("(")
        l.foldLeft(false) {
          (b, v) =>
            if (b) append(", ")
            ConstantTextGen.textGen(v, this)
            true
        }
        append(")")
      }
      case Some(v) => ConstantTextGen.textGen(v, this)
    }
  }
}

case class Call(name : String, parameters : Seq[Expression]) extends Expression {
  def textGen {
    append('"')
    append(name)
    append('"')
    append('(')
    parameters.foldLeft(false){(b, p) =>
      if(b) append(", ")
      append(p)
      true
    }
    append(')')
  }
}

case class ESelect(select : Expression, from : From, where : Option[Expression]) extends Expression {
  def textGen {
    append("(select ")
    append(select)
    indent{
      line{append("from")}
      indent{line{append(from)}}
      if(where.isDefined) {
        line{append("where")}
        indent{line{ append(where.get) }}
      }
      append(')')
    }
  }
}

case class Identity(table : SqlTable) extends Expression {
  def textGen {
    append(param("dialect").map(_.asInstanceOf[SqlDialect].lastIdentityExpression(table)).getOrElse("@@identity"))
  }
}

case class DeclarationRef(name : String) extends Expression {
  def textGen {
    append(name)
  }
}

case class Cast(expression : Expression, as : String) extends Expression {
  def textGen() {
    append("cast(")
    append(expression)
    append(" as ")
    append(as)
    append(")")
  }
}