package ru.apeon.core.entity

import ru.apeon.core._
import loader.Loader
import script._
import javax.naming.{Context, InitialContext}
import sql.{DefaultSqlDialect, SqlDialect}
import xml.NodeSeq

case class DataSource(pack : Package, name : String) extends Statement with Declaration with InPackage {
  def evaluate(env: Environment) {
    env.model.addDataSource(this)
  }

  def dataType(env: Environment) = ScriptDataTypeDataSource()
  def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeDataSource()
  def fillRef(env: Environment, imports: Imports) {}
  def preFillRef(env : Environment, imports: Imports) {}
  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = this
  def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty

  private var loaded = false
  private lazy val _persistentStore : PersistentStore = {
    val xml = Loader.apeonXml.\\("datasource").find(_.\("@name").text == fullName).getOrElse{
      throw new RuntimeException("Datasource \"%s\" nor found in apeon.xml.".format(name))}
    val impl = xml.\("@class").headOption.map{className =>
      Loader.newInstance(className.text).asInstanceOf[DataSourceImpl]}.getOrElse(new DataSourceImplLookup)
    val ret = impl.persistentStore(this, xml)
    ret.load()
    loaded = true
    ret
  }

  def store : PersistentStore = _persistentStore

  def load() {
  }

  def unload() {
    if(loaded) {
      store.unload()
    }
  }
}

trait DataSourceImpl {
  def persistentStore(dataSource : DataSource, xml : NodeSeq) : PersistentStore
}

class DataSourceImplLookup extends DataSourceImpl{
  def persistentStore(dataSource : DataSource, xml : NodeSeq) =
    new SqlPersistentStore(dataSource.fullName,
      new sql.DataSource(
        lookup(dataSource, xml),
        (xml\"@dialect").headOption.map{dialect =>
          Loader.newInstance(dialect.text).asInstanceOf[SqlDialect]
        }.getOrElse(new DefaultSqlDialect)
      ))

  def lookup(dataSource : DataSource, xml : NodeSeq) =
    (new InitialContext).lookup("java:comp/env") match {
      case envContext : Context =>
        envContext.lookup((xml\"@url").headOption.map(_.text).getOrElse(dataSource.fullName)) match {
          case ds : javax.sql.DataSource => ds
          case _ => throw new RuntimeException("Could not get datasource")
        }
      case _ => throw new RuntimeException("Could not get enviroment context")
    }
}