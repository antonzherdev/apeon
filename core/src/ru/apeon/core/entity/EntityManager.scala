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

  def select(select : Select) = {
    val from = select.from.asInstanceOf[FromEntity].entity
    if(!select.columns.isEmpty) throw new RuntimeException("Columns is not empty")

    val ds = select.dataSource
    val store = ds.store
    store.select(select).map{row =>
      val id = new SqlEntityId(ds, from, row(from.primaryKeys.head.name).asInstanceOf[Int])
      var ret = entities.get(id)
      if(ret.isEmpty) {
        val r = row.map{case ((columnName, value)) =>
          from.field(columnName) match {
            case o : ToOne => value match {
              case m : collection.mutable.Map[String, Any] =>{
                val iid = m(o.entity.primaryKeys.head.name)
                if(iid == null) (columnName, null)
                else {
                  val oid = new SqlEntityId(ds, o.entity, iid.asInstanceOf[Int])
                  (columnName, entities.getOrElse(oid, {new Entity(this, oid, m)}))
                }
              }
              case i : Int => (columnName, i)
              case _ => throw new RuntimeException("Not map")
            }
            case _ => (columnName, value)
          }
        }
        ret = Some(new Entity(this, id, r))
      }
     ret.get
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
        val id = e.id.store.insert(Insert(FromEntity(e.id.description, None, DataSourceExpressionDataSource(e.id.dataSource)),
          e.data.map{kv =>
            (e.id.description.field(kv._1), kv._2)
          }.filter(_._2 != null).filter{_._1 match {
            case a : Attribute => !a.isPrimaryKey
            case o : ToOne => {
              val data = e(o).asInstanceOf[Entity]
              if(data.id.isTemporary) {
                afterUpdate(e, o.name, data)
                false
              }
              else true
            }
            case _ => false
          }}.map{kv =>
            InsertColumn(kv._1.name, Const(kv._2))
          }.toSeq
        ))
        e.saveId(id)
      }

      touchedEntities.foreach{kv =>
        val e = kv._1
        val columns = kv._2
        if(!touchedStories.contains(e.id.store)) {
          e.id.store.beginTransaction()
          touchedStories.add(e.id.store)
        }
        e.id.store.update(Update(FromEntity(e.id.description, Some("t"), DataSourceExpressionDataSource(e.id.dataSource)),
          where = Some(e.id.eqlFindById(Some("t"))),
          columns = columns.map{e.id.description.field(_)}.
                  map{column => UpdateColumn(column.name, Const(e.apply(column)))}.toSeq
        ))
      }

      deletedEntities.foreach{e =>
        if(!touchedStories.contains(e.id.store)) {
          e.id.store.beginTransaction()
          touchedStories.add(e.id.store)
        }
        e.id.store.delete(Delete(FromEntity(e.id.description, Some("t"), DataSourceExpressionDataSource(e.id.dataSource)),
          where = Some(e.id.eqlFindById(Some("t")))))
      }

      transactionCounter -= 1
      if(transactionCounter == 0) {
        touchedStories.foreach(_.commit())
        touchedStories.clear()
      }

      insertedEntities.clear()
      touchedEntities.clear()
      deletedEntities.clear()
      nextTempId = -1
    } catch {case e : Exception =>
      touchedStories.foreach(_.rollback())
      throw new RuntimeException(e)
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
      else select( Select(FromEntity(many.entity, Some("m"), DataSourceExpressionDataSource(entity.id.dataSource)),
        where = Some(Equal(Ref("m", many.toOne.name), entity.id.const)))).toSet
    case one : ToOne =>
      data match {
        case id : Int => get(new SqlEntityId(entity.id.dataSource, one.entity, id)).getOrElse(null)
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