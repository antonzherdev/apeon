package ru.apeon.core.entity

import ru.apeon.core._

import collection.Map
import akka.util.{Logger, Logging}
import eql._
import java.sql.{ResultSet, Connection}

trait ReadOnlyPersistentStore {
  def load(){}
  def unload(){}
  def name : String

  def nativeOne(query : String) : Option[Any]

  /**
   * Выполнить запрос
   * @param statement запрос
   * @param parameters параметр
   * @return результат в виде карты, если это select
   */
  def select(select : eql.Select,
             parameters : collection.Map[String, Any] = Map()) : Seq[collection.mutable.Map[String, Any]]

  override def hashCode() = name.hashCode

  override def equals(obj: Any) = obj match {
    case r : ReadOnlyPersistentStore => r.name == this.name
    case _ => false
  }
}

trait PersistentStore extends ReadOnlyPersistentStore{
  def insert(insert : eql.Insert,
             parameters : collection.Map[String, Any] = Map()) : Any

  def update(update : eql.Update,
             parameters : collection.Map[String, Any] = Map())

  def delete(delete : eql.Delete,
             parameters : collection.Map[String, Any] = Map())

  def beginTransaction()
  def commit()

  def rollback()

  def transaction[A](tr : => A) : A = synchronized{
    beginTransaction()
    val ret : A = tr
    commit()
    ret
  }
}

abstract class SqlPersistentStoreBase
        extends PersistentStore with Logging
{
  val readOnlyLog = Logger("ru.apeon.core.entity.ReadOnlyPersistentStore")
  protected val _eql = new Eql

  class Eql extends eql.Eql {
    override def getConnection = SqlPersistentStoreBase.this.getConnection
    override def dialect = SqlPersistentStoreBase.this.dialect
    override def generator = SqlPersistentStoreBase.this.generator
    override def closeConnection() {
      SqlPersistentStoreBase.this.closeConnection(connection)
    }
  }


  def nativeOne(query: String) = _eql.transaction{
    val stm : java.sql.Statement = _eql.connection.createStatement
    readOnlyLog.debug(query)
    val rs : ResultSet = stm.executeQuery(query)
    if(!rs.next)
      None
    else
      Some(rs.getObject(1))
   }

  def closeConnection(connection : Connection) {
    connection.close()
  }
  def getConnection : Connection
  def dialect : sql.SqlDialect
  def generator : SqlGenerator

  def select(select: eql.Select, parameters: Map[String, Any]) = {
    readOnlyLog.debug("<%s> %s", name, select)
    _eql.select(select, parameters).toSeqMutableMap
  }


  def update(update: Update, parameters: Map[String, Any]) {
    log.debug("<%s> %s", name, update)
    _eql.update(update, parameters)
  }

  def insert(insert: Insert, parameters: Map[String, Any]) = {
    log.debug("<%s> %s", name, insert)
    _eql.insert(insert, parameters)
  }

  def delete(delete: Delete, parameters: Map[String, Any]) {
    log.debug("<%s> %s", name, delete)
    _eql.delete(delete, parameters)
  }

  def beginTransaction() {
    _eql.beginTransaction()
  }
  def rollback() {
    _eql.rollback()
  }
  def commit() {
    _eql.commit()
  }

}

class SqlPersistentStore(val name : String,
                         val dataSource : sql.DataSource = sql.SqlConfiguration.dataSource,
                         val generator : SqlGenerator = new SqlGenerator) extends SqlPersistentStoreBase {
  def getConnection = dataSource.getConnection

  def dialect = dataSource.dialect
}
