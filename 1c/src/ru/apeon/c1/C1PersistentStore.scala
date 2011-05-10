package ru.apeon.c1

import ru.apeon.core.entity.{SqlPersistentStoreBase}
import java.sql.DriverManager
import ru.apeon.core.eql.SqlGenerator
import ru.apeon.core._
import sql.Column
import akka.util.Logging

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
            case "JAVA_OBJECT" => rs.getString(j + 1)
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