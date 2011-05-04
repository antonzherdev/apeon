package ru.apeon.core.sql

abstract class Statement {
  override def toString = DefaultSqlDialect.toString(this, Map())
  def toString(parameters : scala.collection.Map[String, Any]) = DefaultSqlDialect.toString(this, parameters)
}

case class Select(
        from : From,
        columns: Seq[SelectColumn] = Seq(),
        where : Option[Expression] = None,
        orderBy : Seq[OrderBy] = Seq())
        extends Statement

abstract class SelectColumn {
  def name : Option[String]
}

class Column(val expression : Expression, val name : Option[String] = None)  extends SelectColumn {
  override def toString = "%s as %s".format(expression, name)

  override def equals(obj: Any) = obj match {
    case col : Column => col.expression == expression && col.name == name
    case _ => false
  }
}

object Column {
  def apply(expression : Expression, name : Option[String]) = new Column(expression, name)
  def apply(expression : Expression) = new Column(expression, None)
  def apply(expression : Expression, name : String) = new Column(expression, Some(name))
}

case class ColumnSeq(columns : Seq[SelectColumn], seqName : String)  extends SelectColumn{
  def name = Some(seqName)
}


abstract class From {
  def name : String
  def join : Option[Join]

  def fromOption(alias : String) : Option[From] =
    if(name == alias) Some(this)
    else join match {
      case Some(j) => j.table.fromOption(alias)
      case None => None
    }

  def from(alias : String) = fromOption(alias).get

  def setJoin(join : Option[Join]) : From

  def setJoin(join : Join) : From = setJoin(Some(join))

  def setLastJoin(lastJoin : Join) : From = join match {
    case Some(j) => setJoin(j.setLastJoin(lastJoin))
    case None => setJoin(lastJoin)
  }

  def lastJoin : From = join match {
    case Some(j) => j.table.lastJoin
    case None => this
  }

  def froms : Seq[From] = join match {
    case None => Seq(this)
    case Some(j) => this +: j.table.froms
  }
}

object From {
  def apply(name : String) : FromTable = FromTable(SqlTable("", name))
  def apply(name : String, alias : String) : FromTable = FromTable(SqlTable("", name), Some(alias))
  def apply(name : String, join : Join) : FromTable = FromTable(SqlTable("", name), join = Some(join))
  def apply(name : String, alias : String, join : Join) : FromTable = FromTable(SqlTable("", name), Some(alias), Some(join))
}


case class FromTable(table : SqlTable, alias : Option[String] = None, join : Option[Join] = None) extends From {
  def name = alias match {
    case Some(a) => a
    case None => table.name
  }
  def setJoin(join: Option[Join]) = FromTable(table, alias, join)
}

abstract class Join {
  val table : From
  def setJoin(join : Option[Join]) : Join = setTable(table.setJoin(join))
  def setLastJoin(join : Join) : Join = setTable(table.setLastJoin(join))
  def setTable(table : From) : Join
}

case class CrossJoin(table : From) extends Join {
  def setTable(table : From) = CrossJoin(table)
}

abstract class OuterJoin(val name : String) extends Join {
  def on : Expression
}

case class InnerJoin(table : From, on : Expression) extends OuterJoin("inner") {
  def setTable(table : From) = InnerJoin(table, on)
}

case class LeftJoin(table : From, on : Expression) extends OuterJoin("left") {
  def setTable(table : From) = LeftJoin(table, on)
}

case class OrderBy(expression : Expression, direction : Direction = Asc())

abstract class Direction
case class Asc() extends Direction {
  override def toString = "asc"
}
case class Desc() extends Direction {
  override def toString = "desc"
}

case class Insert(table : SqlTable, columns : Seq[InsertColumn]) extends Statement

case class InsertColumn(column : String, exp : Expression)

case class Update(from : FromTable, columns : Seq[UpdateColumn], where : Option[Expression] = None) extends Statement

case class UpdateColumn(column : String, exp : Expression)

case class Delete(from : FromTable, where : Option[Expression] = None) extends Statement

case class StatementList(statements : Seq[Statement]) extends Statement

case class Declare(name : String, dataType : SqlDataType) extends Statement

case class Set(ref : String, expression : Expression) extends Statement

case class SimpleSelect(columns: Seq[SelectColumn] = Seq()) extends Statement