package ru.apeon.core.entity

import ru.apeon.core._
import script._
import javax.naming.{Context, InitialContext}
import sql.ComtecAsaSqlDialect

/**
 * @author Anton Zherdev
 */

case class DataSource(pack : Package, name : String) extends Statement with Declaration with InPackage {
  def evaluate(env: Environment) {
    pack.model.addDataSource(this)
  }

  def dataType(env: Environment) = ScriptDataTypeDataSource()
  def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeDataSource()

  private lazy val _persistentStore : PersistentStore = new SqlPersistentStore(fullName, new sql.DataSource((new InitialContext).lookup("java:comp/env") match {
        case envContext : Context => envContext.lookup("datasource/" + name) match {
          case ds : javax.sql.DataSource => ds
          case _ => throw new RuntimeException("Could not get datasource")
        }
        case _ => throw new RuntimeException("Could not get enviroment context")
      }, new ComtecAsaSqlDialect))

  def store : PersistentStore = _persistentStore

  def fillRef(env: Environment, imports: Imports) {}
  def preFillRef(model: ObjectModel, imports: Imports) {}

  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = this

  def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty

  override def toString = fullName
}