package ru.apeon.core.sql

abstract class Statement extends TextGen

case class Select(
        from : From,
        columns: Seq[SelectColumn] = Seq(),
        where : Option[Expression] = None,
        orderBy : Seq[OrderBy] = Seq())
        extends Statement
{
  def textGen {
    line{append("select")}
    columns match {
      case Seq() => append(" *")
      case _ => indent{append(columns, ",")}
    }
    line{append("from")}
    indent{line{ append(from) }}

    if(where.isDefined) {
      line{append("where")}
      indent{line{ append(where) } }
    }
    if(!orderBy.isEmpty) {
      line{append("order by")}
      indent{line{append(orderBy, ", ")}}
    }
    append(';')
  }
}

abstract class SelectColumn extends TextGen {
  def name : Option[String]
  def textGen(aliasPref : String)
}

class Column(val expression : Expression, val name : Option[String] = None)  extends SelectColumn {
  def textGen = textGen("")

  def textGen(aliasPref: String) {
    line {
      append(expression)
      if (name.isDefined) {
        append(" as \"")
        if (!aliasPref.isEmpty) {
          append(aliasPref)
          append('.')
        }
        append(name.get)
        append("\"")
      }
    }
  }

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
  def textGen = textGen("")

  def textGen(aliasPref: String) = indent{
    val pref = if(!aliasPref.isEmpty) "%s.%s".format(aliasPref, seqName) else seqName

    columns.foldLeft(false) { (b, i) =>
      if(b) append(",")
      i.sb = this.sb
      i.textGen(pref)
      i.sb = null
      true
    }
  }
}


abstract class From extends TextGen{
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

  def textGenDeclaration()

  def textGen {
    textGenDeclaration()
    append(join)
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

  def textGenDeclaration() {
    append(table)
    option(" as \"", alias, "\"")
  }

  def setJoin(join: Option[Join]) = FromTable(table, alias, join)
}

abstract class Join extends TextGen {
  val table : From
  def setJoin(join : Option[Join]) : Join = setTable(table.setJoin(join))
  def setLastJoin(join : Join) : Join = setTable(table.setLastJoin(join))
  def setTable(table : From) : Join
}

case class CrossJoin(table : From) extends Join {
  def textGen {
    line {
      append("cross join ")
      append(table)
    }
  }

  def setTable(table : From) = CrossJoin(table)
}

abstract class OuterJoin(name : String) extends Join {
  def on : Expression

  def textGen {
    line {
      append(name)
      append(" join ")
      table.sb = sb
      table.textGenDeclaration()
      table.sb = null
      append(" on ")
      append(on)
      append(table.join)
    }
  }
}

case class InnerJoin(table : From, on : Expression) extends OuterJoin("inner") {
  def setTable(table : From) = InnerJoin(table, on)
}

case class LeftJoin(table : From, on : Expression) extends OuterJoin("left") {
  def setTable(table : From) = LeftJoin(table, on)
}

case class OrderBy(expression : Expression, direction : Direction = Asc()) extends TextGen {
  def textGen {
    append(expression)
    append(" ")
    append(direction)
  }
}

abstract class Direction
case class Asc() extends Direction {
  override def toString = "asc"
}
case class Desc() extends Direction {
  override def toString = "desc"
}

case class Insert(table : SqlTable, columns : Seq[InsertColumn]) extends Statement
{
  def textGen {
    line{
      append("insert into ")
      append(table)
      append("(")
      columns.foldLeft(false){ (b, c) =>
        if(b) append(", ")
        append('"')
        append(c.column)
        append('"')
        true
      }
      append(')')
    }
    line{
      append("values(")
      columns.foldLeft(false){ (b, c) =>
        if(b) append(", ")
        append(c.exp)
        true
      }
      append(')')
      append(';')
    }
  }
}

case class InsertColumn(column : String, exp : Expression)

case class Update(from : FromTable, columns : Seq[UpdateColumn], where : Option[Expression] = None)
        extends Statement
{
  def textGen {
    line{
      append("update ")
      append(from)
    }
    line{
      append("set")
    }
    indent{
      columns.foldLeft(false) {(b, col) =>
        if(b) append(",")
        append(col)
        true
      }
    }
    if(where.isDefined) {
      line{append("where")}
      indent{line{ append(where) } }
    }
    append(';')
  }
}

case class UpdateColumn(column : String, exp : Expression) extends TextGen{
  def textGen {
    line {
      append('"')
      append(column)
      append('"')
      append(" = ")
      append(exp)
    }
  }
}

case class Delete(from : FromTable, where : Option[Expression] = None) extends Statement {
  def textGen {
    line{
      append("delete from ")
      append(from.table)
    }
    if(from.alias.isDefined) {
      line{
        append("from ")
        append(from)
      }
    }
    if(where.isDefined) {
      line{append("where")}
      indent{line{ append(where) } }
    }
    append(';')
  }
}

case class StatementList(statements : Seq[Statement]) extends Statement {
  def textGen {
    line{
      append("begin")
    }
    statements.foreach(statement => append(statement))
    line{
      append("end;")
    }
  }
}

case class Declare(name : String, dataType : SqlDataType) extends Statement {
  def textGen {
    line{
      append("declare ")
      append(name)
      append(" ")
      append(dataType.toString)
      append(";")
    }
  }
}

case class Set(ref : String, expression : Expression) extends Statement{
  def textGen {
    line{
      append("set ")
      append(ref)
      append(" = ")
      append(expression)
      append(";")
    }
  }
}

case class SimpleSelect(columns: Seq[SelectColumn] = Seq()) extends Statement {
  def textGen {
    line {
      append("select ")
      append(columns, ", ")
      append(";")
    }
  }
}