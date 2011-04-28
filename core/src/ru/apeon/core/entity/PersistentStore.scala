package ru.apeon.core.entity

import ru.apeon.core._

import collection.Map
import akka.util.{Logger, Logging}
import eql.{SqlGenerator, Update, Insert, Delete}

trait ReadOnlyPersistentStore {
  def name : String

  /**
   * Выполнить запрос
   * @param statement запрос
   * @param parameters параметр
   * @return результат в виде карты, если это select
   */
  def select(select : eql.Select,
             parameters : collection.Map[String, Any] = Map()) : Seq[collection.mutable.Map[String, Any]]
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
}

class SqlPersistentStore(val name : String,
                         val dataSource : sql.DataSource = sql.SqlConfiguration.dataSource,
                         val generator : SqlGenerator = new SqlGenerator)
        extends PersistentStore with Logging
{
  val readOnlyLog = Logger("ru.apeon.core.entity.ReadOnlyPersistentStore")
  val e = new eql.Eql(){
    override def dataSource = SqlPersistentStore.this.dataSource
    override def generator = SqlPersistentStore.this.generator
  }

  def select(select: eql.Select, parameters: Map[String, Any]) = e.transaction{
    readOnlyLog.debug("<%s> %s", name, select)
    e.select(select, parameters).toSeqMutableMap
  }


  def update(update: Update, parameters: Map[String, Any]) {
    log.debug("<%s> %s", name, update)
    e.update(update, parameters)
  }

  def insert(insert: Insert, parameters: Map[String, Any]) = {
    log.debug("<%s> %s", name, insert)
    e.insert(insert, parameters)
  }

  def delete(delete: Delete, parameters: Map[String, Any]) {
    log.debug("<%s> %s", name, delete)
    e.delete(delete, parameters)
  }

  def beginTransaction() {
    e.beginTransaction()
  }
  def rollback() {
    e.rollback()
  }
  def commit() {
    e.commit()
  }

}