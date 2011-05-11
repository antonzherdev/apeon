package ru.apeon.c1

import ru.apeon.core.entity.{SqlPersistentStoreBase}
import java.sql.DriverManager
import ru.apeon.core._
import eql.{Expression, ConstObject, SqlGenerator}
import sql.Column
import akka.util.Logging
import com.ipc.oce.objects._OCCommonRef

class C1PersistentStore(val name : String, val url : String, val userName : String, val password : String)
        extends SqlPersistentStoreBase with Logging
{
  val connection = {
    Class.forName("com.ipc.oce.jdbc.OCEDriver")
    DriverManager.getConnection(url, userName, password)
  }

  def getConnection = connection
  val generator = new SqlGenerator
  val dialect = new C1SqlDialect

  override protected val _eql = new Eql{
    override protected def sqlRow(select: eql.Select, sqlSelect: sql.Select, rows: sql.Rows) =
      new sql.RowSyntax(rows.rs, sqlSelect) {
        override def value(column: Column, j: Int) = {
          rs.getMetaData.getColumnTypeName(j + 1) match {
            case "JAVA_OBJECT" => C1Ref(rs.getString(j + 1), rs.getObject(j + 1).asInstanceOf[_OCCommonRef])
            case _ => super.value(column, j)
          }
        }
      }
  }

  override def unload() {
    log.debug("Closing 1c connection.")
    connection.close()
  }
}

case class C1Ref(uuid : String, ref : _OCCommonRef) extends ConstObject{
  override def toString = uuid

  override def hashCode() = uuid.hashCode

  def expression = ConstRef(this)
}

case class ConstRef(ref : C1Ref) extends Expression