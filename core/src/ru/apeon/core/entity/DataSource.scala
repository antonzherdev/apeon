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

  private lazy val xml = Loader.apeonXml.\\("datasource").find(_.\("@name").text == fullName).getOrElse{
    throw new RuntimeException("Datasource \"%s\" nor found in apeon.xml.".format(name))}
  private lazy val impl =
    xml.\("@class").headOption.map{className =>
      Loader.loadClass(className.text).getConstructor(classOf[DataSource]).newInstance(this).asInstanceOf[DataSourceImpl]
    }.getOrElse(new DataSourceImplLookup(this))

  private var loaded = false
  private lazy val _persistentStore : PersistentStore = {
    val ret = impl.persistentStore(xml)
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

  def insert(em : EntityManager, entity : Entity) {
    impl.insert(em, entity)
  }

  def update(em : EntityManager, entity : Entity, columns: collection.Set[String]) {
    impl.update(em, entity, columns)
  }

  def delete(em : EntityManager, entity : Entity) {
    impl.delete(em, entity)
  }
}

