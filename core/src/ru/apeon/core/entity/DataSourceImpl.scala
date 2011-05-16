package ru.apeon.core.entity

import xml.NodeSeq
import ru.apeon.core.sql
import sql.{DefaultSqlDialect, SqlDialect}
import ru.apeon.core.loader.Loader
import javax.naming.{Context, InitialContext}
import ru.apeon.core.eql._

abstract class DataSourceImpl {
  val dataSource : DataSource
  def persistentStore(xml : NodeSeq) : PersistentStore
  def insert(em : EntityManager, e: Entity) {
    val id = e.id.store.insert(Insert(FromEntity(e.id.description, None, DataSourceExpressionDataSource(e.id.dataSource)),
      e.data.map{kv =>
        (e.id.description.field(kv._1), kv._2)
      }.filter(_._2 != null).filter{kv =>
        kv._1 match {
          case a : Attribute => !a.isPrimaryKey
          case o : ToOne => {
            if(kv._2.isInstanceOf[Entity]) {
              val data = kv._2.asInstanceOf[Entity]
              if(data.id.isTemporary) {
                em.afterUpdate(e, o.name, data)
                false
              }
              else true
            } else true
          }
          case _ => false
        }}.map{kv =>
        InsertColumn(kv._1.name, Const(kv._2))
      }.toSeq
    ))
    e.saveId(id)
  }

  def update(em : EntityManager, e: Entity, columns: collection.Set[String]) {
    e.id.store.update(Update(FromEntity(e.id.description, Some("t"), DataSourceExpressionDataSource(e.id.dataSource)),
      where = Some(e.id.eqlFindById(Some("t"))),
      columns = columns.map {
        e.id.description.field(_)
      }.map {
        column => UpdateColumn(column.name, Const(e.apply(column)))
      }.toSeq
    ))
  }

  def delete(em : EntityManager, e: Entity) {
    e.id.store.delete(Delete(FromEntity(e.id.description, Some("t"), DataSourceExpressionDataSource(e.id.dataSource)),
          where = Some(e.id.eqlFindById(Some("t")))))
  }
}

class DataSourceImplLookup(val dataSource : DataSource) extends DataSourceImpl{
  def persistentStore(xml : NodeSeq) =
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