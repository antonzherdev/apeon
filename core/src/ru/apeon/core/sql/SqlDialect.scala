package ru.apeon.core.sql

import java.text.{SimpleDateFormat, DateFormat}
import java.util.Date

trait SqlDialect {
  def createTableStatement(table : SqlTable) : String
  def createColumnStatement(column : SqlColumn) : String
  def modifyColumnStatement(column : SqlColumn) : String
  def columnDefinitionStatement(column : SqlColumn) : String
  def createForeignKeyStatement(key : SqlForeignKey) : String
  def lastIdentityExpression(table : SqlTable) : String
  def toString(o : Object, parameters : scala.collection.Map[String, Any]) : String
}

class DefaultSqlDialect extends SqlDialect with TextGen {
  val schema : String = ""
  val catalog : String = null

  //TODO: Колонка id не является обязательной. Нужно добавлять таблицу без колонки id. Может быть, добавлять пустую колонку или выполнять проверку только с одной уже известной колонкий. Добавлять pk нужно уже при добавлении соответствующей колонки.
  def createTableStatement(table : SqlTable) : String =
    """create table %s(id integer primary key default autoincrement)""".format(table)

  def createColumnStatement(column : SqlColumn) =
    """alter table %s add %s;""".format(column.table, columnDefinitionStatement(column))

  def modifyColumnStatement(column : SqlColumn) =
    """alter table %1$s modify %2$s;alter table %1$s modify "%3$s" %4$s;%5$s""".format(
      column.table, columnDefinitionStatement(column),
      column.name, if(column.nulls) "null" else "not null",
      column.default match {
        case Some(s) => ""
        case None => """alter table %s alter "%s" drop default""".format(column.table, column.name)
      }
    )

  def columnDefinitionStatement(column : SqlColumn) =
    "\"" + column.name + "\" " +
            column.dataType + " " +
            (if(column.nulls) "null" else "not null") +
            (if(column.default.isDefined) " default " + column.default.get else "")



  def createForeignKeyStatement(key : SqlForeignKey) =
    """alter table %s add foreign key "%s"(%s) references %s(%s) on update %s on delete %s""".format(
      key.foreign.table,
      key.name,
      key.foreign.columnQuote,
      key.primary.table,
      key.primary.columnQuote,
      key.onUpdate,
      key.onDelete
    )

  def lastIdentityExpression(table : SqlTable) = "@@identity"

  protected def appendFunction = {
    case s : Select => {
      line{append("select")}
      s.columns match {
        case Seq() => append(" *")
        case _ => indent{append(s.columns, ",")}
      }
      line{append("from")}
      indent{line{ append(s.from) }}

      if(s.where.isDefined) {
        line{append("where")}
        indent{line{ append(s.where) } }
      }
      if(!s.orderBy.isEmpty) {
        line{append("order by")}
        indent{line{append(s.orderBy, ", ")}}
      }
      append(';')
    }
    case c : Column => appendSelectColumn(c)
    case c : ColumnSeq => appendSelectColumn(c)
    case f : From => {
      appendFrom(f)
      append(f.join)
    }
    case j : CrossJoin => line {
      append("cross join ")
      append(j.table)
    }
    case j : OuterJoin => line {
      append(j.name)
      append(" join ")
      appendFrom(j.table)
      append(" on ")
      append(j.on)
      append(j.table.join)
    }
    case o : OrderBy => {
      append(o.expression)
      append(" ")
      append(o.direction)
    }

    case i : Insert => {
      line{
        append("insert into ")
        append(i.table)
        append("(")
        i.columns.foldLeft(false){ (b, c) =>
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
        i.columns.foldLeft(false){ (b, c) =>
          if(b) append(", ")
          append(c.exp)
          true
        }
        append(')')
        append(';')
      }
    }

    case u : Update => {
      line{
        append("update ")
        append(u.from)
      }
      line{
        append("set")
      }
      indent{
        u.columns.foldLeft(false) {(b, col) =>
          if(b) append(",")
          append(col)
          true
        }
      }
      if(u.where.isDefined) {
        line{append("where")}
        indent{line{ append(u.where) } }
      }
      append(';')
    }
    case c : UpdateColumn => line {
      appendEscape()
      append(c.column)
      appendEscape()
      append(" = ")
      append(c.exp)
    }

    case d : Delete => {
      line{
        append("delete from ")
        append(d.from.table)
      }
      if(d.from.alias.isDefined) {
        line{
          append("from ")
          append(d.from)
        }
      }
      if(d.where.isDefined) {
        line{append("where")}
        indent{line{ append(d.where) } }
      }
      append(';')
    }

    case l : StatementList => {
      line{
        append("begin")
      }
      l.statements.foreach(statement => append(statement))
      line{
        append("end;")
      }
    }
    case d : Declare => line{
      append("declare ")
      append(d.name)
      append(" ")
      append(d.dataType.toString)
      append(";")
    }
    case s : Set => line{
      append("set ")
      append(s.ref)
      append(" = ")
      append(s.expression)
      append(";")
    }
    case s : SimpleSelect => line {
      append("select ")
      append(s.columns, ", ")
      append(";")
    }

    case n : ConstNull => append("null")
    case b : BinaryExpression => {
      appendBinaryPart(b, b.left)
      append(' ')
      append(binaryExpressionAlias(b))
      append(' ')
      appendBinaryPart(b, b.right)
    }
    case n : Not => {
      append("not(")
      append(n.expression)
      append(")")
    }
    case e : Exists => {
      append("exists(select * from")
      indent{
        indent(line(append(e.from)))
        if(e.where.isDefined) {
          line{append("where")}
          indent{line{append(e.where.get)}}
        }
      }
      line{append(')')}
    }
    case r : Ref => {
      if(r.from.isDefined) {
        appendEscape()
        append(r.from.get)
        appendEscape()
        append('.')
      }
      appendEscape()
      append(r.column)
      appendEscape()
    }
    case c : ConstNumeric => appendConstantValue(c.value)
    case c : ConstString => appendConstantValue(c.value)
    case c : ConstDate => appendConstantValue(c.value)

    case p : Parameter => {
      param(p.name) match {
        case None => {
          append(':')
          append(p.name)
        }
        case Some(l: List[_]) => {
          append("(")
          l.foldLeft(false) {
            (b, v) =>
              if (b) append(", ")
              appendConstantValue(v)
              true
          }
          append(")")
        }
        case Some(v) => appendConstantValue(v)
      }
    }
    case c : Call => {
      appendEscape()
      append(c.name)
      appendEscape()
      append('(')
      c.parameters.foldLeft(false){(b, p) =>
        if(b) append(", ")
        append(p)
        true
      }
      append(')')
    }
    case e : ESelect => {
      append("(select ")
      append(e.select)
      indent{
        line{append("from")}
        indent{line{append(e.from)}}
        if(e.where.isDefined) {
          line{append("where")}
          indent{line{ append(e.where.get) }}
        }
        append(')')
      }
    }
    case i : Identity => {
      append(lastIdentityExpression(i.table))
    }
    case r : DeclarationRef => append(r.name)
    case t : SqlTable => {
      if(!t.schema.isEmpty) {
        appendEscape()
        append(t.schema)
        appendEscape()
        append('.')
      }
      appendEscape()
      append(t.name)
      appendEscape()
    }
  }

  var dateFormat : DateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")

  def appendConstantValue(v : Any) {
    v match {
      case s: String => {
        append('\'')
        s.foreach {
          c: Char =>
            c match {
              case '\'' => {
                append('\'')
                append('\'')
              }
              case _ => append(c)
            }
        }
        append('\'')
      }
      case d: Date => {
        append('\'')
        append(dateFormat.format(d))
        append('\'')
      }
      case _ => append(v)
    }
  }

  def appendEscape() {append('"')}

  def binaryExpressionAlias(binaryExpression : BinaryExpression) : String = binaryExpression match {
    case e : Equal => e.right match {
      case p : Parameter => param(p.name) match  {
        case Some(l : List[_]) => "in"
        case _ => "="
      }
      case n : ConstNull => "is"
      case _ => "="
    }
    case e : NotEqual => e.right match {
      case p : Parameter => param(p.name) match  {
        case Some(l : List[_]) => "not in"
        case _ => "<>"
      }
      case n : ConstNull => "is not"
      case _ => "<>"
    }
    case _ => binaryExpression.alias
  }

  def appendBinaryPart(binaryExpression : BinaryExpression, part : Expression) {
    binaryExpression match {
      case and : And => part match {
        case or: Or => {
          append('(')
          append(or)
          append(')')
        }
        case _ => append(part)
      }
      case _ => append(part)
    }
  }

  def appendFrom(ff: From) {
    ff match {
      case f : FromTable => {
        append(f.table)
        if(f.alias.isDefined) {
          append(" as ")
          appendEscape()
          append(f.alias.get)
          appendEscape()
        }
      }
    }
  }

  def appendSelectColumn(col : SelectColumn, aliasPref : String = "") {
    col match {
      case c : Column => {
        line {
          append(c.expression)
          if (c.name.isDefined) {
            append(" as ")
            appendEscape()
            if (!aliasPref.isEmpty) {
              append(aliasPref)
              append('.')
            }
            append(c.name.get)
            appendEscape()
          }
        }
      }
      case c : ColumnSeq => {
        indent{
          val pref = if(!aliasPref.isEmpty) "%s.%s".format(aliasPref, c.seqName) else c.seqName

          c.columns.foldLeft(false) { (b, i) =>
            if(b) append(",")
            appendSelectColumn(i, pref)
            true
          }
        }
      }
    }
  }

}

object DefaultSqlDialect extends DefaultSqlDialect

class AsaSqlDialect extends DefaultSqlDialect {
  override val schema = "dba"

  override def createTableStatement(table: SqlTable) =
    """create table %1$s(id integer default 0 primary key);
 insert into %1$s(id) values(0);""".format(table)
}

