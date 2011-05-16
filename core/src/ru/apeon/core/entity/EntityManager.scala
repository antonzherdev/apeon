package ru.apeon.core.entity

import ru.apeon.core.eql._
import collection.mutable.Buffer
import akka.util.Logging
import ru.apeon.core.script.{ObjectModel}

trait EntityManager {
  def model : ObjectModel

  /**
   * Получить сущность
   * @param id идентификатор
   */
  def get(id : EntityId) : Option[Entity]

  /**
   * Выборка.
   * @param select запрос без секции columns
   * @param dataSource хранилище
   */
  def select(select : Select): Seq[Entity]

  //TODO: загрузка сразу нескольких сущностей. Batch load
  def lazyLoad(entity : Entity, field : Field, data : Any) : Any

  def beginTransaction()
  def commit()

  def insert(description : Description) : Entity = insert(description, description.dataSource)
  def insert(description : Description, source : DataSource) : Entity

  /**
   * Зарегистрировать сущность в менеджере
   */
  def register(entity : Entity)

  def beforeUpdate(entity : Entity, key : String, data : Any)
  def afterUpdate(entity : Entity, key : String, data : Any)

  def afterInsert(entity : Entity)

  def beforeDelete(entity : Entity)
  def afterDelete(entity : Entity)

  def transaction[A](tr : => A) : A = {
    beginTransaction()
    val ret : A = tr
    commit()
    ret
  }

  def toEntity(ds: DataSource, description: Description, data: collection.mutable.Map[String, Any]): Entity
}

class DefaultEntityManager(val model : ObjectModel = EntityConfiguration.model) extends EntityManager with Logging {
  protected val entities = collection.mutable.Map[EntityId, Entity]()
  protected val touchedEntities = collection.mutable.Map[Entity, Set[String]]()
  protected val insertedEntities = Buffer[Entity]()
  protected val deletedEntities = collection.mutable.Set[Entity]()
  protected val touchedStories = collection.mutable.Set[PersistentStore]()
  protected var transactionCounter = 0

  def get(id : EntityId) : Option[Entity] =
    entities get id match {
      case Some(ret) => Some(ret)
      case None => select(
        Select(from = FromEntity(id.description, dataSourceExpression = DataSourceExpressionDataSource(id.dataSource)),
          where = Some(id.eqlFindById(None)))) match
      {
        case Seq() => None
        case Seq(e : Entity) => Some(e)
        case _ => throw new RuntimeException("Find many entity but must only one.")
      }
    }

  def toEntity(ds: DataSource, description: Description, data: collection.mutable.Map[String, Any]): Entity = {
    val id = description.primaryKeys match {
      case Seq(pk) => new OneEntityId(ds, description, data(pk.name))
      case Seq() => throw new RuntimeException("No primary key for entity \"%s\".".format(description.fullName))
      case pks => new MultiEntityId(ds, description, pks.map {
        pk => data(pk.name)
      })
    }
    var ret = entities.get(id)
    if (ret.isEmpty) {
      val r = data.map {
        case ((columnName, value)) =>
          description.field(columnName) match {
            case o: ToOne => value match {
              case m: collection.mutable.Map[String, Any] => {
                val iid = m(o.entity.primaryKeys.head.name)
                if (iid == null) (columnName, null)
                else {
                  val oid = new OneEntityId(ds, o.entity, iid)
                  (columnName, entities.getOrElse(oid, {
                    new Entity(this, oid, m)
                  }))
                }
              }
              case _ => (columnName, value)
            }
            case _ => (columnName, value)
          }
      }
      ret = Some(new Entity(this, id, r))
    }
    ret.get
  }

  def select(select : Select) = {
    val from = select.from.asInstanceOf[FromEntity].entity
    if(!select.columns.isEmpty) throw new RuntimeException("Columns is not empty")

    val ds = select.dataSource
    val store = ds.store
    store.transaction{
      store.select(select).map{row =>
        toEntity(ds, from, row)
      }
    }
  }

  def beginTransaction() {
    transactionCounter += 1
  }

  def checkTransaction() {
    if(transactionCounter == 0) {
      throw new RuntimeException("Transaction is not open")
    }
  }

  def commit() {
    try{
      if(log.logger.isDebugEnabled) {
        val sb = new StringBuilder
        sb.append("Commit")
        if(!insertedEntities.isEmpty) {
          sb.append("\nInserted:\n")
          sb.append(insertedEntities.mkString("\n"))
        }
        if(!touchedEntities.isEmpty) {
          sb.append("\nTouched:\n")
          sb.append(touchedEntities.mkString("\n"))
        }
        if(!deletedEntities.isEmpty) {
          sb.append("\nDeleted:\n")
          sb.append(deletedEntities.mkString("\n"))
        }
        log.debug(sb.toString())
      }

      insertedEntities.foreach{e =>
        if(!touchedStories.contains(e.id.store)) {
          e.id.store.beginTransaction()
          touchedStories.add(e.id.store)
        }
        touchedEntities.remove(e)
        e.id.dataSource.insert(this, e)
      }

      touchedEntities.foreach{kv =>
        val e = kv._1
        val columns = kv._2
        if (!touchedStories.contains(e.id.store)) {
          e.id.store.beginTransaction()
          touchedStories.add(e.id.store)
        }
        e.id.dataSource.update(this, e, columns.map(column => e.id.description.field(column).asInstanceOf[FieldWithSource]))
      }

      deletedEntities.foreach{e =>
        if(!touchedStories.contains(e.id.store)) {
          e.id.store.beginTransaction()
          touchedStories.add(e.id.store)
        }
        e.id.dataSource.delete(this, e)
      }

      transactionCounter -= 1
      if(transactionCounter == 0) {
        touchedStories.foreach(_.commit())
        touchedStories.clear()
      }
    } catch {
      case e : Throwable => {
        touchedStories.foreach{store =>
          try{
            store.rollback()
          } catch{ case er : Throwable =>
            log.error(er, "Error while rollback")
          }
        }
        touchedStories.clear()
        transactionCounter = 0
        throw new RuntimeException(e)
      }}
    finally {
      insertedEntities.clear()
      touchedEntities.clear()
      deletedEntities.clear()
      nextTempId = -1
    }
  }

  def register(entity : Entity) {
    entities.update(entity.id, entity)
  }

  private var nextTempId = -1

  def insert(description : Description, dataSource : DataSource) : Entity = {
    val b = collection.mutable.Map.newBuilder[String, Any]
    for(field <- description.fields) {
      val value : Any = field match {
        case s : FieldWithSource => s.default match {
          case Some(DefaultInt(i)) => i
          case Some(DefaultString(s)) => s
          case None => null
        }
        case _=> Set()
      }
      b += (field.name -> value)
    }
    val ret = new Entity(this, TemporaryEntityId(dataSource, description, nextTempId), b.result())
    nextTempId -= 1
    afterInsert(ret)
    ret
  }

  def lazyLoad(entity : Entity, field : Field, data : Any) : Any = field match {
    case many : ToMany =>
      if(entity.id.isTemporary) Set()
      else entity.id.dataSource.lazyLoad(this, entity, many)
    case one : ToOne =>
      data match {
        case id : Int => get(new OneEntityId(entity.id.dataSource, one.entity, id)).getOrElse(null)
        case _ => throw new RuntimeException("Data not int")
      }
    case _ =>
      throw new RuntimeException("Not support lazy load for not to one and not to many in field \"%s\" for \"%s\"".format(field, entity))
  }

  def beforeUpdate(entity: Entity, key: String, data: Any) {}

  def afterUpdate(entity: Entity, key: String, data: Any) {
    if(entity.id.description.field(key).isInstanceOf[FieldWithSource]) {
      touchedEntities.update(entity, touchedEntities.getOrElse(entity, Set()) + key)
    }
  }

  def afterInsert(entity: Entity) {
    insertedEntities.append(entity)
  }

  def beforeDelete(entity: Entity) {}

  def afterDelete(entity: Entity) {
    if(insertedEntities.contains(entity)) {
      insertedEntities.remove(insertedEntities.indexOf(entity))
    } else {
      deletedEntities.add(entity)
    }
    touchedEntities.remove(entity)
  }
}