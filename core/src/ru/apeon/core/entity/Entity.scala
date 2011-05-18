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
      val upd = value match {
        case v : Entity =>
          if(v.id == e.id) {
            false
          } else {
            if(v.id.equalInheritance(e.id)) {
              if(v.id.description.isInstanceOf(e.id.description)) {
                _data.update(column, value)
              }
              false
            }
            else {
              true
            }
          }
        case _ => e.id.equalAny(value)
      }
      if(upd) doUpdate(column, value)
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
    this.id.description.primaryKeys match {
      case Seq(pk) => {
        _id  = this.id match {
          case i : TemporaryEntityId => {
            OneEntityId(i.dataSource, i.description, id)
          }
          case d =>
            throw new RuntimeException("Save id for not temporary id, for %s".format(d))
        }
        _data.update(pk.name, id)
      }
      case pks => {
        _id  = this.id match {
          case i : TemporaryEntityId => {
            MultiEntityId(i.dataSource, i.description, id.asInstanceOf[Seq[Any]])
          }
          case d =>
            throw new RuntimeException("Save id for not temporary id, for %s".format(d))
        }
        val pkI = pks.iterator
        val idI = id.asInstanceOf[Seq[Any]].iterator
        while(pkI.hasNext) {
          _data.update(pkI.next.name, idI.next)
        }
      }
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

  def data : Seq[Any]

  def equalAny(id : Any)  : Boolean

  override def equals(obj: Any) = obj match {
    case id : EntityId => id.dataSource == dataSource && id.description == description && equalId(id)
    case _ => false
  }

  def equalInheritance(id : EntityId) : Boolean =
    (id.description.isInstanceOf(description) || description.isInstanceOf(id.description)) && equalId(id)

  def equalId(id : EntityId) : Boolean
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

  def equalId(id: EntityId) = id match {
    case t : TemporaryEntityId => t.id == this.id
    case _ => false
  }

  def data = Seq(id)
}

case class OneEntityId(dataSource : DataSource, description : Description, id : Any) extends EntityId {
  def isTemporary = false

  override def toString = id.toString

  override def equals(obj: Any) = obj match {
    case sqlId : OneEntityId => sqlId.id == id && sqlId.dataSource == dataSource && sqlId.description == description
    case _ => false
  }

  def eqlFindById(alias : Option[String]) = alias match {
    case Some(a) => Equal(Dot(Ref(a), Ref(description.primaryKeys.head.name)), Const(id))
    case None => Equal(Ref(description.primaryKeys.head.name), Const(id))
  }

  def const = Const(id)

  override def hashCode = ((629 + description.hashCode)*37 + dataSource.hashCode)*37 + id.hashCode

  def equalAny(id: Any) = id match {
    case i : OneEntityId => i == this
    case _ => id == this.id
  }

  def equalId(id: EntityId) = id match {
    case t : OneEntityId => t.id == this.id
    case _ => false
  }

  def data = Seq(id)
}

case class MultiEntityId(dataSource : DataSource, description : Description, ids : Seq[Any]) extends EntityId {
  def isTemporary = false

  override def toString = ids.mkString("<", ",", ">")

  override def equals(obj: Any) = obj match {
    case sqlId : MultiEntityId => sqlId.ids.corresponds(this.ids){_ == _} && sqlId.dataSource == dataSource && sqlId.description == description
    case _ => false
  }

  def eqlFindById(alias : Option[String]) = {
    var ret : Option[Expression] = None
    val i = ids.iterator
    for(pk <- description.primaryKeys) {
      val e = Equal(alias.map{a => Dot(a, pk.name)}.getOrElse(Ref(pk.name)), Const(i.next()))
      ret = ret match {
        case None => Some(e)
        case Some(r) => Some(And(r, e))
      }
    }
    ret.get
  }

  def const = throw new RuntimeException("Const is not supported for multi identity")

  override def hashCode = ((629 + description.hashCode)*37 + dataSource.hashCode)*37 + ids.hashCode

  def equalAny(id: Any) = id match {
    case i : MultiEntityId => i == this
    case s : Seq[Any] => s.corresponds(ids) {_ == _}
    case _ => id == this.ids
  }

  def equalId(id: EntityId) = id match {
    case t : MultiEntityId => t.ids.corresponds(this.ids){_ == _}
    case _ => false
  }

  def data = ids
}