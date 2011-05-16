package ru.apeon.core.sql

import java.sql.{ResultSet, Connection}
import scala.Iterator
import collection.mutable.ListBuffer
import collection._
import java.lang.String
import akka.util.{Logger}
import java.math.MathContext

/**
 * @author Anton Zherdev
 */

trait SqlReadOnly {
  def dataSource : DataSource = SqlConfiguration.dataSource
  def dialect : SqlDialect = dataSource.dialect
  val readOnlyLog = Logger("ru.apeon.core.sql.SqlReadOnly")
  var connection : Connection = null
  protected var connectionCounter = 0

  def beginTransaction() {
    if (connection == null) {
      connection = dataSource.getConnection
    }
    connectionCounter += 1
  }

  def commit() {
    connectionCounter -= 1
    if(connectionCounter <= 0) {
      connection.commit()
      connection.close()
      connection = null
      connectionCounter = 0
    }
  }

  def rollback() {
    checkTransaction()
    connection.rollback()
    connection.close()
    connection = null
    connectionCounter = 0
  }


  def transaction[A](tr : => A) : A = synchronized{
    if(connection == null) {
      beginTransaction()
      val ret : A = tr
      commit()
      ret
    }
    else tr
  }

  def anyToString(v : Any) : String = v match {
    case s : String => "'" + s.replace("'", "''") + "'"
    case m : Map[String, Any] => m("id").toString
    case _ => v.toString
  }

  def applyParameters(sql : String, parameters : collection.Map[String, Any]) : String = {
    var ret : String = sql
    parameters.foreach{kv =>
      val (k, v) = kv
      ret = ret.replace(":" + k, anyToString(v))
    }
    ret
  }


  def selectOne(sql : String, parameters : collection.Map[String, Any]): Option[Row] = {
    checkTransaction()
    val stm : java.sql.Statement = connection.createStatement
    val rs : ResultSet = stm.executeQuery(applyParameters(sql, parameters))
    if(!rs.next) return None
    Some(new RowSimple(rs))
  }

  def selectOne(sql : Select, parameters : collection.Map[String, Any]): Option[Row] = {
    checkTransaction()
    val stm : java.sql.Statement = connection.createStatement
    val s : String = sql.toString(parameters)
    readOnlyLog.debug(s)
    val rs : ResultSet = stm.executeQuery(s)
    if(!rs.next) return None
    Some(new RowSyntax(rs, sql))
  }

  implicit def toMap(v : (String, Any) ) : immutable.Map[String, Any] = immutable.Map(v._1 -> v._2)

  implicit def cellAsString(r : Cell) : String = r match {
    case CellData(d) => d.toString
    case CellNull() =>  ""
  }

  implicit def cellAsInt(r : Cell) : Int = r match {
    case CellData(d : Int) => d
    case CellData(_) => throw new SqlError("Cell is not integer")
    case CellNull() =>  0
  }

  implicit def cellAsBoolean(r : Cell) : Boolean = r match {
    case CellData(d : Boolean) => d
    case CellData(d : Int) => d != 0
    case CellData(s : String) => s == "1"
    case CellData(_) => throw new SqlError("Cell is not boolean")
    case CellNull() =>  false
  }

  def checkTransaction() {
    if (connection == null) {
      throw new SqlError("Transaction have not been opened")
    }
  }

  def select(sql : String): Rows = select(sql, Map[String, Any]())

  def select(sql : String, parameters : collection.Map[String, Any]) : Rows = {
    checkTransaction()
    val stm = connection.createStatement
    val rs : ResultSet = stm.executeQuery(applyParameters(sql, parameters))
    new RowsSimple(rs)
  }

  def pars(parameters : collection.Map[String, Any]) = parameters + ("dialect" -> dialect)

  def select(sql : Select) : Rows = select(sql, immutable.Map[String, Any]())
  def select(sql : Select, parameters : collection.Map[String, Any]) : Rows = {
    checkTransaction()
    val stm = connection.createStatement
    val s : String = sql.toString(pars(parameters))
    readOnlyLog.debug(s)
    val rs : ResultSet = stm.executeQuery(s)
    new RowsSyntax(rs, sql)
  }

  def isEmpty[A](values : A*) : A = {
    for(value <- values.iterator) value match {
      case c : CellData => if(!c.isEmpty) return value
      case _ => return value
    }
    return values.last
  }
}

trait Sql extends SqlReadOnly {
  val log = Logger("ru.apeon.core.sql.Sql")

  def insert(table : SqlTable, sql : String) : Int = insert(table, sql, immutable.Map.empty)

  def insert(table : SqlTable, sql : String, parameters : collection.Map[String, Any]) : Int = {
    checkTransaction()
    connection.createStatement.execute(applyParameters(sql, parameters))
    lastIdentity(table)
  }

  def lastIdentity(table : SqlTable) : Int = {
    val rs : ResultSet = connection.createStatement.executeQuery("select " + dialect.lastIdentityExpression(table))
    if(!rs.next) throw new SqlError("Could not get last identity")
    rs.getInt(1)
  }

  def insert(sql : Insert, parameters : collection.Map[String, Any]) : Int = {
    checkTransaction()
    val s : String = sql.toString(pars(parameters))
    log.debug(s)
    connection.createStatement.execute(s)
    lastIdentity(sql.table)
  }

  def execute(sql : Statement, parameters : collection.Map[String, Any]) : Option[ResultSet] = {
    checkTransaction()
    val s : String = sql.toString(pars(parameters))
    log.debug(s)
    val stm = connection.createStatement
    val ret = if(stm.execute(s)) Some(stm.getResultSet)
    else None

    ret
  }
}

trait Row extends Iterable[Cell] {
  def toMutableMap : mutable.Map[String, Any]

  def apply(column : String) : Cell
  def apply(column : Int) : Cell
}

abstract class RowDecorator(val parent : Row) extends Row{
  def iterator = parent.iterator

  def toMutableMap = parent.toMutableMap

  def apply(column: String) = parent.apply(column)

  def apply(column: Int) = parent.apply(column)
}

abstract class AbstractRow extends Row {
  val rs : ResultSet

  def apply(column : String) : Cell = rs.getObject(column) match {
    case null => CellNull()
    case x : Any => CellData(x)
  }
  def apply(column : Int) : Cell = rs.getObject(column + 1) match {
    case null => CellNull()
    case x : Any => CellData(x)
  }

  def iterator = new Iterator[Cell] {
    var i : Int = 0

    def next() = {
      if(!hasNext) throw new NoSuchElementException("next on empty Iterator")
      val ret : Cell = AbstractRow.this(i)
      i = i + 1
      ret
    }
    def hasNext = rs.getMetaData.getColumnCount > i
  }
}

class RowSimple(val rs : ResultSet) extends AbstractRow {
  def toMutableMap = {
    val m : mutable.Map[String, Any] = mutable.Map.empty
    var i : Int = 0
    while(i < rs.getMetaData.getColumnCount) {
      m += (rs.getMetaData.getColumnLabel(i + 1) -> (rs.getObject(i + 1) match {
        case d : java.math.BigDecimal => BigDecimal(d)
        case d => d
      }))
      i += 1
    }
    m
  }
}

class RowSyntax(rs : ResultSet, val sql : Select) extends RowSimple(rs) {
  override def toMutableMap = toMutableMap(ColumnSeq(sql.columns, ""), rs, 0)._1

  def toMutableMap(column : ColumnSeq, rs : ResultSet, i : Int) :  (mutable.Map[String, Any], Int) = {
    var j = i
    val m = mutable.Map.empty[String, Any]
    column.columns.foreach{col => col match {
      case named : Column => {
        val v = rs.getObject(j + 1) match {
          case d : java.math.BigDecimal => BigDecimal(d, new MathContext(rs.getMetaData.getScale(j + 1)))

          //TODO: Это проблема ASA(Глюк #27). Если строка пустая, то она возращает Null. Нужно, чтобы это только для нее и выполнялось.
          case null => if(rs.getMetaData.getColumnClassName(j + 1) == "java.lang.String") "" else null

          case d => d
        }
        m += (named.name.getOrElse{rs.getMetaData.getColumnLabel(j + 1)} -> v)
        j += 1
      }
      case seq : ColumnSeq => {
        val r = toMutableMap(seq, rs, j)
        m += (seq.name.get -> r._1)
        j = r._2
      }
    }}
    (m, j)
  }
}


abstract class Cell {
  def isNull : Boolean
  def isEmpty : Boolean

  def orIfIsNull(cell : => Cell) : Cell = if(isNull) cell else this
  def orIfIsEmpty(cell : => Cell) : Cell = if(isEmpty) cell else this
}

case class CellData(data : Any) extends Cell {
  def isNull = false
  def isEmpty = data match {
    case s : String => s.isEmpty
    case _ => false
  }

  override def equals(obj: Any) = obj match {
    case CellData(d) => d == data
    case _ => obj == data
  }

  override def toString : String = data.toString
}
case class CellNull() extends Cell {
  def isNull = true
  def isEmpty = true

  override def equals(obj: Any) = obj == null || obj == CellNull
}

trait Rows extends Iterable[Row] {
  def toSeqMutableMap : Seq[mutable.Map[String, Any]]

  final def map[B](f: Row => B): Seq[B] = {
    val b = new ListBuffer[B]

    val i : Iterator[Row] = iterator
    while (i.hasNext) {
      b += f(i.next())
    }
    b.toSeq
  }

  def createRow : Row
}

abstract class AbstractRows extends Rows {
  val rs : ResultSet

  def iterator = new Iterator[Row] {
    var row : Row = createRow
    var has_next : Int = 0

    def next() = {
      if(has_next != 1) {
        if(!rs.next) throw new NoSuchElementException("next on empty Iterator")
      }
      has_next = 0
      row
    }
    def hasNext = {
      if(has_next == 0) {
        if(rs.next) {
          has_next = 1
          true
        }
        else {
          has_next = -1
          false
        }

      }
      else has_next == 1
    }
  }

  def toSeqMutableMap : Seq[mutable.Map[String, Any]] = map{r : Row => r.toMutableMap}
}

class RowsSimple(val rs : ResultSet) extends AbstractRows {
  def createRow = new RowSimple(rs)
}

class RowsSyntax(val rs : ResultSet, val sql : Select) extends AbstractRows {
  def createRow = new RowSyntax(rs, sql)
}

abstract class RowsDecorator(val parent : Rows) extends Rows{
  def iterator = parent.iterator

  def toSeqMutableMap = parent.toSeqMutableMap

  def createRow = parent.createRow
}

case class SqlError(s : String) extends Exception(s)