package ru.apeon.sync

import org.scalatest.matchers.ShouldMatchers
import ru.apeon.core._
import entity._
import eql.Insert
import collection.Map
import script._
import org.scalatest.{Spec}

class SyncSpec extends Spec with ShouldMatchers with EntityDefine with ScriptTest {
  SyncListener.preLoad()
  val M = collection.mutable.Map

  val col1 = Attribute(pack, "col1", "col1", AttributeDataTypeInteger())
  val col2 = Attribute(pack, "col2", "col2", AttributeDataTypeInteger())
  val conses = ToManyRef(pack, "conses", "Cons", "article")
  val article = desc("Article").decl(Id, col1, col2, conses).b

  val articleCol = ToOne(pack, "article", "article", "Article")
  val uid = Attribute(pack, "uid", "uid", AttributeDataTypeInteger())
  val cons = desc("Cons").decl(Id, articleCol, uid, col1).b

  fillRef()
  //TODO: Тест синхронизации коллекции сущностей
  describe("Sync") {
    it("Изменение") {
      var ok = false
      run(
        new TestEntityManager() {
          override def get(id: EntityId) = Some(new Entity(this, id, M("id" -> 1, "col1" -> 2, "col2" -> 11)))

          val id2 = new OneEntityId(dataSource, article, 2)
          override def select(select: eql.Select) =
            Seq(new Entity(this, id2, M("id" -> 2, "col1" -> 2, "col2" -> 22)))

          override def afterUpdate(entity: Entity, key: String, data: Any) {
            entity.id should equal(id2)
            key should equal("col2")
            data should equal(11)
            ok = true
          }
        },
        ref("Article", 1) ~ ref("sync",  Eql("s.col1 = d.col1")))
      ok should equal(true)
    }
   it("Добавление") {
      var ok1 = false
      var ok2 = false
      var ok3 = false
      run(
        new TestEntityManager() {
          override def get(id: EntityId) = Some(new Entity(this, id, M("id" -> 1, "col1" -> 2, "col2" -> 11)))

          override def select(select: eql.Select) =
            Seq()

          override def afterUpdate(entity: Entity, key: String, data: Any) {
            key match {
              case "col1" =>
                data should equal(2)
                ok1 = true
              case "col2" =>
                if(ok3) {
                  data should equal(16)
                  ok2 = true
                } else {
                  data should equal(11)
                  ok3 = true
                }
              case _ => throw new RuntimeException("Error")
            }
          }

          override def insert(description: Description, dataSource: DataSource) =
            new Entity(this, new OneEntityId(new DataSource(pack, "ds") {
              override def store = new SqlPersistentStore("test"){
                override def insert(insert: Insert, parameters: Map[String, Any]) = 20
              }
            } , description, 0))
        },

        ref("Article", 1) ~ ref("sync", Eql("s.col1 = d.col1"), bf(
          (ref("d") ~ ref("col1")) := (ref("s") ~ ref("col1")),
          (ref("d") ~ ref("col2")) := (ref("s") ~ ref("col2")) + 5
        ))
      )
      ok1 should equal(true)
      ok2 should equal(true)
    }

    it("ToMany") {
      run(
        new TestEntityManager() {
          override def get(id: EntityId) = id.asInstanceOf[OneEntityId].id match {
            case 1 => Some(new Entity(this, id, M("id" -> 1, "col1" -> 2, "col2" -> 11)))
            case 2 => Some(new Entity(this, id, M("id" -> 2, "col1" -> 2, "col2" -> 11)))
          }

          def id(v : Int) = new OneEntityId(dataSource, cons, v)
          def ee(m : collection.mutable.Map[String, Any]) = new Entity(this, id(m("id").asInstanceOf[Int]), m)
          override def select(select: eql.Select) = {
            val f = select.from.asInstanceOf[eql.FromEntity]
            f.entity should equal(cons)
            select.where.get match {
              case eql.And(eql.Equal(eql.Dot(eql.Ref("d"), eql.Ref("uid")), eql.Const(1)),
              eql.Equal(eql.Dot(eql.Ref("d"), eql.Ref("article")), eql.Const(2))) =>
                Seq(ee(M("id" -> 3, "article" -> 2, "uid" -> 1, "col1" -> 11)))

              case eql.And(eql.Equal(eql.Dot(eql.Ref("d"), eql.Ref("uid")), eql.Const(2)),
              eql.Equal(eql.Dot(eql.Ref("d"), eql.Ref("article")), eql.Const(2))) => Seq()

              case _ => throw new RuntimeException("Unknown where %s".format(select.where.get))
            }
          }


          override def lazyLoad(entity: Entity, field : Field, data : Any) = {
            field.name should equal("conses")
            entity.id.asInstanceOf[OneEntityId].id match {
              case 1 => Seq(
                ee(M("id" -> 1, "article" -> 1, "uid" -> 1, "col1" -> 22)),
                ee(M("id" -> 2, "article" -> 1, "uid" -> 2, "col1" -> 32)))
              case 2 => Seq(
                ee(M("id" -> 3, "article" -> 2, "uid" -> 1, "col1" -> 11)),
                ee(M("id" -> 4, "article" -> 2, "uid" -> 3, "col1" -> 16)))
              case _ => throw new RuntimeException("Unknown id")
            }
          }

          override def insert(description: Description, dataSource: DataSource) =
            new Entity(this, new OneEntityId(new DataSource(pack, "db") {
              override def store = new SqlPersistentStore("test"){
                override def insert(insert: Insert, parameters: Map[String, Any]) = 5
              }
            }, description, 0))
        },
        Val("article1", ref("Article", 1)),
        Val("article2", ref("Article", 2)),
        (ref("article2")~ref("conses")) := (ref("article1")~ref("conses")~ref("sync", Eql("d.uid = s.uid"))),
        ref("article2")~ref("conses")).asInstanceOf[Seq[Entity]]
    }

  }
}