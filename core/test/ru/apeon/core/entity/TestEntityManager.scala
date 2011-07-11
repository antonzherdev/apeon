package ru.apeon.core.entity

import ru.apeon.core.eql
import collection.mutable.Map
import ru.apeon.core.script.{DefaultEnvironment}

class TestEntityManager extends DefaultEntityManager {
  override def beginTransaction() {}
  override def select(select: eql.Select) : Seq[Entity] = Seq()
  override def register(entity: Entity) {}
  override def lazyLoad(entity: Entity, field : Field, data : Any) : Any = null
  override def get(id: EntityId) : Option[Entity] = None
  override def commit() {}
  override def beforeUpdate(entity: Entity, key: String, data: Any) {}
  override def beforeDelete(entity: Entity) {}
  override def afterUpdate(entity: Entity, key: String, data: Any) {}
  override def afterInsert(entity: Entity) {}
  override def afterDelete(entity: Entity) {}

  implicit def toSome(entity : Entity) : Option[Entity] = Some(entity)

  private def map(id: EntityId, data: Seq[(String, Any)]): Map[String, Any] = {
    val m = Map[String, Any](data: _*)
    if (id.isInstanceOf[OneEntityId]) {
      m.update(id.description.primaryKeys.head.name, id.data)
    }
    m
  }

  def e(id : EntityId, data : (String, Any)*) = {
    new Entity(this, id, map(id, data))
  }
  def e(description : Description, id : Any, data : (String, Any)*) = {
    val _id = new OneEntityId(description.dataSource, description, id)
    new Entity(this, _id, map(_id, data))
  }
}

class Env(override val em : EntityManager) extends DefaultEnvironment {
  override protected def createEntityManager = null
}