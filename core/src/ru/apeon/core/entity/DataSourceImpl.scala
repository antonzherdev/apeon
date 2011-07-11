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
  protected def toInsertColumns(em: EntityManager, e: Entity): Seq[InsertColumn] = {
    e.data.map {
      kv =>
        (e.id.description.field(kv._1), kv._2)
    }.filter(_._2 != null).filter {
      kv =>
        kv._1 match {
          case a: Attribute => !a.isPrimaryKey
          case o: ToOne => {
            if (kv._2.isInstanceOf[Entity]) {
              val data = kv._2.asInstanceOf[Entity]
              if (data.id.isTemporary) {
                em.afterUpdate(e, o.name, data)
                false
              }
              else true
            } else true
          }
          case _ => false
        }
    }.map {
      kv =>
        InsertColumn(kv._1.name, Const(kv._2))
    }.toSeq
  }

  def insert(em : EntityManager, e: Entity) {
    val id = e.id.store.insert(Insert(FromEntity(e.id.description, None, DataSourceExpressionDataSource(e.id.dataSource)),
      toInsertColumns(em, e)
    ))
    e.saveId(id)
  }

  def update(em : EntityManager, e: Entity, fields: collection.Set[FieldWithSource]) {
    e.id.store.update(Update(FromEntity(e.id.description, Some("t"), DataSourceExpressionDataSource(e.id.dataSource)),
      where = Some(e.id.eqlFindById(Some("t"))),
      columns = fields.map {
        column => UpdateColumn(column.name, Const(e.data(column.name)))
      }.toSeq
    ))
  }

  def delete(em : EntityManager, e: Entity) {
    e.id.store.delete(Delete(FromEntity(e.id.description, Some("t"), DataSourceExpressionDataSource(e.id.dataSource)),
          where = Some(e.id.eqlFindById(Some("t")))))
  }


  def lazyLoad(em : EntityManager, entity : Entity, many : ToMany) : Set[Entity] =
    em.select( Select(FromEntity(many.entity, Some("m"), DataSourceExpressionDataSource(dataSource)),
        where = Some(Equal(Dot(Ref("m"), Ref(many.one)), entity.id.const)))).toSet

  def lazyLoad(em : EntityManager, entity : Entity, one : ToOne, data : Any) : Entity = data match {
    case id : Int => em.get(new OneEntityId(entity.id.dataSource, one.entity, id)).getOrElse(null)
    case _ => throw new RuntimeException("Data not int")
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