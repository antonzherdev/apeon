package ru.apeon.core.sql

import java.io.PrintWriter
import java.lang.{String, Class}

/**
 * @author Anton Zherdev
 */

class DataSource(val dataSource : javax.sql.DataSource, val dialect : SqlDialect = SqlConfiguration.dialect) extends javax.sql.DataSource {
  val checker = new SqlChecker(dialect)

  def getConnection(userName: String, password: String) = dataSource.getConnection(userName, password)

  def getConnection = dataSource.getConnection

  def setLogWriter(out: PrintWriter) {dataSource.setLogWriter(out)}

  def setLoginTimeout(seconds: Int) {dataSource.setLoginTimeout(seconds)}

  def getLogWriter = dataSource.getLogWriter

  def getLoginTimeout = dataSource.getLoginTimeout

  def unwrap[T](iface: Class[T]) = dataSource.unwrap(iface)

  def isWrapperFor(iface: Class[_]) = dataSource.isWrapperFor(iface)
}