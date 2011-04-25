package ru.apeon.core.entity

import ru.apeon.core.eql._

/**
 * Сама сущность.
 * @param id идентификатор сущности
 * @param _data данные
 */
class Entity(val manager : EntityManager,
             private var _id : EntityId,
             private val _data : collection.mutable.Map[String, Any] = collection.mutable.Map[String, Any]()) {
  manager.register(this)

  def id = _id

  def apply(columnName : String) : Any = apply(id.description.field(columnName))
  def update(column : Field, d : Any) : Entity  = update(column.name, d)

  private def lazyUpdate(column : Field, data : Any = null) = {
    val l = manager.lazyLoad(this, column, data)
    _data.update(column.name, l)
    l
  }

  def apply(column : Field) : Any = {
    val ret = _data.get(column.name)
    column match {
      case toOne : ToOne => ret match {
        case Some(e : Entity) => e
        case Some(null) => null
        case Some(id) => lazyUpdate(column, id)
        case None => lazyUpdate(column)
      }
      case _ => {
        if(ret.isDefined) ret.get
        else lazyUpdate(column)
      }
    }
  }

  def update(column : String, value : Any) : Entity = _data.get(column) match {
    case None => {
      doUpdate(column, value)
      this
    }
    case Some(e : Entity) => {
      if(!(value match {
        case v : Entity => e == v
        case _ => e.id.equalAny(value)
      })) {
        doUpdate(column, value)
      }
      this
    }
    case Some(e) => {
      if(!(value match {
        case v : Entity => v.id.equalAny(e)
        case _ => e == value
      })) {
        doUpdate(column, value)
      }
      this
    }

  }

  def data : collection.Map[String, Any] = _data

  protected def doUpdate(columnName : String, value : Any) {
    manager.beforeUpdate(this, columnName, value)
    id.description.field(columnName) match {
      case many : ToMany => {
        val entities = value.asInstanceOf[Traversable[Entity]]
        entities.foreach{_.update(many.toOne, this)}
        apply(columnName) match {
          case s : Traversable[Entity] => s.foreach{e =>
            if(entities.find(_.id == e.id).isEmpty) e.delete()
          }
          case null => {}
        }
        _data.update(columnName, value)
      }
      case _ => _data.update(columnName, value)
    }
    manager.afterUpdate(this, columnName, value)
  }

  override def toString = id.description.toString(this)

  /**
   * Перевод в строковое представление по-умолчанию.
   * Этот перевод работает, если в сущности не определена функция toString
   */
  def defaultToString = "Entity(%s, %s)".format(id, _data.toSeq.map( v =>
      v._1 + " = " + (v._2 match {
        case e : Entity => e.id.toString
        case _ => v._2
      })
    ).mkString(", "))

  override def equals(obj: Any) = obj match {
    case ent : Entity => ent.id == id && ent.manager == manager
    case _ => id == obj
  }

  def toMap : collection.Map[String, Any] = _data


  protected var _deleted = false
  def deleted = _deleted

  def delete() {
    manager.beforeDelete(this)
    _deleted = true
    manager.afterDelete(this)
  }

  def saveId(id : Any) {
    _id  = this.id match {
      case i : TemporaryEntityId => {
        SqlEntityId(i.dataSource, i.description, id.asInstanceOf[Int])
      }
    }
    this.id.description.primaryKeys match {
      case Seq(pk) => _data.update(pk.name, id)
    }
  }

  def copy(description : Description = id.description, dataSource : DataSource = id.dataSource) : Entity = {
    val ret = manager.insert(description, dataSource)
    val copyOnlyAttributes = id.dataSource != dataSource
    _data.foreach{col =>
      val field = description.fieldOption(col._1)
      if(field.isDefined) {
        if(field.get match {
          case att : Attribute => true
          case toOne : ToOne => copyOnlyAttributes
          case toMany : ToMany => false
        }) {
          ret.update(field.get, col._2)
        }
      }
    }
    ret
  }

  override def hashCode = id.hashCode
}

/**
 * Идентификатор сущности
 */
trait EntityId {
  /**
   * Хранилище сущностей
   */
  def store : PersistentStore = dataSource.store

  def dataSource : DataSource

  /**
   * Описание сущности
   */
  def description : Description

  /**
   * Временный идентификатор. True, если строчка еще не добавлена в базу данных
   */
  def isTemporary : Boolean

  def eqlFindById(alias : Option[String] = None) : Expression
  def const : Expression

  def equalAny(id : Any)  : Boolean
}

case class TemporaryEntityId(dataSource : DataSource, description : Description, id : Int) extends EntityId {
  def isTemporary = true
  def equalAny(id: Any) = id match {
    case i : TemporaryEntityId => i == this
    case _ => id == this.id
  }

  def eqlFindById(alias: Option[String]) =
    throw new RuntimeException("Find for temporaty id for \"%s\" in datasource \"%s\".".format(
      description.fullName, dataSource.fullName))

  def const = throw new RuntimeException("Const for temporaty id.")

  override def equals(obj: Any) = obj match {
    case sqlId : SqlEntityId => sqlId.id == id && sqlId.dataSource == dataSource && sqlId.description.table == description.table
    case _ => false
  }
}

case class SqlEntityId(dataSource : DataSource, description : Description, id : Int) extends EntityId {
  def isTemporary = false

  override def toString = id.toString

  override def equals(obj: Any) = obj match {
    case sqlId : SqlEntityId => sqlId.id == id && sqlId.dataSource == dataSource && sqlId.description.table == description.table
    case _ => false
  }

  def eqlFindById(alias : Option[String]) = Equal(Ref(alias, description.primaryKeys.head.name), ConstNumeric(id))

  def const = ConstNumeric(id)

  override def hashCode = ((31 + description.table.hashCode)*31 + dataSource.hashCode)*17 + id

  def equalAny(id: Any) = id match {
    case i : SqlEntityId => i == this
    case _ => id == this.id
  }
}