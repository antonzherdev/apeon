package ru.apeon.core.entity

import ru.apeon.core.eql._

/**
 * Сама сущность.
 * @param id идентификатор сущности
 * @param _data данные
 */
class Entity(val manager : EntityManager,
             var id : EntityId,
             private val _data : collection.mutable.Map[String, Any] = collection.mutable.Map[String, Any]()) {
  manager.register(this)

  def apply(field : String) : Any = apply(id.description.field(field))
  def path(fields : String*) : Any =
    if(fields.tail.isEmpty) apply(fields.head)
    else apply(fields.head).asInstanceOf[Entity].path(fields.tail : _*)

  private def lazyUpdate(column : Field, data : Any = null) = {
    val l = manager.lazyLoad(this, column, data)
    _data.update(column.name, l)
    l
  }

  def append(many : ToMany, entity : Entity) {
    apply(many) match {
      case es : Set[Entity] => _data.update(many.name, es + entity)
      case es : Seq[Entity] => _data.update(many.name, es :+ entity)
      case null => _data.update(many.name, Set(entity))
    }
  }

  def apply(field : Field) : Any = {
    val ret = _data.get(field.name)
    field match {
      case toOne : ToOne => ret match {
        case Some(e : Entity) => e
        case Some(null) => null
        case Some(idValue) => lazyUpdate(field, idValue)
        case None => lazyUpdate(field)
      }
      case _ => {
        if(ret.isDefined) ret.get
        else lazyUpdate(field)
      }
    }
  }

  def update(fieldName : String, value : Any) {
    update(id.description.field(fieldName), value)
  }

  def update(field : Field, value : Any) {
    _data.get(field.name) match {
      case None => {
        doUpdate(field, value)
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
                  _data.update(field.name, value)
                }
                false
              }
              else {
                true
              }
            }
          case _ => e.id.equalAny(value)
        }
        if(upd) doUpdate(field, value)
        this
      }
      case Some(e) => {
        if(!(value match {
          case v : Entity => v.id.equalAny(e)
          case _ => e == value
        })) {
          doUpdate(field, value)
        }
        this
      }
    }
  }

  def data : collection.Map[String, Any] = _data

  protected def doUpdate(field: Field, value: Any) {
    manager.beforeUpdate(this, field.name, value)
    field match {
      case many: ToMany => {
        val entities = value.asInstanceOf[Traversable[Entity]]
        entities.foreach {
          _.update(many.one, this)
        }
        apply(field) match {
          case s: Traversable[Entity] => s.foreach {
            e =>
              if (entities.find(_.id.hashCode() == e.id.hashCode()).isEmpty) e.delete()
          }
          case null => {}
        }
        _data.update(field.name, value)
      }
      case _ => _data.update(field.name, value)
    }
    manager.afterUpdate(this, field.name, value)
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
        this.id = this.id match {
          case i : TemporaryEntityId => {
            OneEntityId(i.dataSource, i.description, id)
          }
          case d =>
            throw new RuntimeException("Save id for not temporary id, for %s".format(d))
        }
        _data.update(pk.name, id)
      }
      case pks => {
        this.id = this.id match {
          case i : TemporaryEntityId => {
            MultiEntityId(i.dataSource, i.description, id.asInstanceOf[Seq[Any]])
          }
          case d =>
            throw new RuntimeException("Save id for not temporary id, for %s".format(d))
        }
        val pkI = pks.iterator
        val idI = id.asInstanceOf[Seq[Any]].iterator
        while(pkI.hasNext) {
          _data.update(pkI.next().name, idI.next())
        }
      }
    }
  }

  def copy(description : Description = id.description, dataSource : DataSource = id.dataSource) : Entity = {
    val ret = manager.insert(description, dataSource)
    val copyOnlyAttributes = id.dataSource != dataSource
    description.fields.foreach{col =>
      val field = description.fieldOption(col.name)
      if(field.isDefined) {
        field.get match {
          case att : Attribute => ret.update(field.get, apply(col))
          case toOne : ToOne => if(!copyOnlyAttributes) {
            ret.update(field.get, apply(col))
          }
          case toMany : ToMany =>
            if(description == id.description) {
              ret.update(field.get,
                apply(col).asInstanceOf[Traversable[Entity]].map{e =>
                  val r = e.copy(e.id.description, dataSource)
                  r.update(toMany.one, ret)
                  r
                })
            }
        }
      }
    }
    ret
  }

  override def hashCode = id.hashCode

  def hashCode(fields : Seq[Seq[String]]) = {
    fields.foldLeft(17){case (ret, p) =>
      ret*32 + path(p : _*).hashCode()
    }
  }
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

  override def equals(obj: Any) = obj.hashCode == this.hashCode

  def equalInheritance(id : EntityId) : Boolean =
    (id.description.isInstanceOf(description) || description.isInstanceOf(id.description)) && equalId(id)

  def equalId(id : EntityId) : Boolean

  def idFor(desc : Description) : EntityId
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

  def idFor(desc: Description) = TemporaryEntityId(dataSource, desc, id)

  override val hashCode = ((113 + description.hashCode)*37 + dataSource.hashCode)*37 + id
}

case class OneEntityId(dataSource : DataSource, description : Description, id : Any) extends EntityId {
  def isTemporary = false

  override def toString = id.toString

  def eqlFindById(alias : Option[String]) = alias match {
    case Some(a) => Equal(Dot(Ref(a), Ref(description.primaryKeys.head.name)), Const(id))
    case None => Equal(Ref(description.primaryKeys.head.name), Const(id))
  }

  def const = Const(id)

  override val hashCode = ((149 + description.hashCode)*37 + dataSource.hashCode)*37 + id.hashCode

  def equalAny(id: Any) = id match {
    case i : OneEntityId => i == this
    case _ => id == this.id
  }

  def equalId(id: EntityId) = id match {
    case t : OneEntityId => t.id == this.id
    case _ => false
  }

  def data = Seq(id)

  def idFor(desc: Description) = OneEntityId(dataSource, desc, id)
}

case class MultiEntityId(dataSource : DataSource, description : Description, ids : Seq[Any]) extends EntityId {
  def isTemporary = false

  override def toString = ids.mkString("<", ",", ">")

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

  override val hashCode = ((619 + description.hashCode)*37 + dataSource.hashCode)*37 + ids.hashCode

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

  def idFor(desc: Description) = MultiEntityId(dataSource, desc, ids)
}