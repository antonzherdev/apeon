package ru.apeon.core.script

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import ru.apeon.core._
import entity._
import eql.Insert
import collection.Map

/**
 * @author Anton Zherdev
 */

class SyncSpec extends Spec with ShouldMatchers with EntityDefine with ScriptDefine{
  def ps = EntityConfiguration.dataSource
  val M = collection.mutable.Map
  val sh = new DefaultObjectModel
  val pack = Package("ru.apeon.core.test")
  val ds = new DataSource(pack, "ds")
  sh.addDataSource(ds)

  val col1 = Attribute(pack, "col1", "col1", AttributeDataTypeInteger())
  val col2 = Attribute(pack, "col2", "col2", AttributeDataTypeInteger())
  val conses = ToManyRef(pack, "conses", "Cons", "article")
  val article = Description(pack, "Article", "ds", Table("dba", "article"), Seq(Id, col1, col2, conses))
  sh.addEntityDescription(article)

  val articleCol = ToOne(pack, "article", "article", "Article")
  val uid = Attribute(pack, "uid", "uid", AttributeDataTypeInteger())
  val cons = Description(pack, "Cons", "ds", Table("dba", "cons"), Seq(Id, articleCol, uid, col1))
  sh.addEntityDescription(cons)
  EntityConfiguration.model = sh
  FillRef(sh, pack, pack, article, cons)

  def run(em : EntityManager, statement : Statement*) = Script(sh, pack, statement : _*).evaluate(new Env(em))

  describe("Sync") {
    it("Изменение") {
      var ok = false
      run(
        new EmptyEntityManager() {
          override def get(id: EntityId) = Some(new Entity(this, id, M("id" -> 1, "col1" -> 2, "col2" -> 11)))

          val id2 = new SqlEntityId(ps, article, 2)
          override def select(select: eql.Select) =
            Seq(new Entity(this, id2, M("id" -> 2, "col1" -> 2, "col2" -> 22)))

          override def afterUpdate(entity: Entity, key: String, data: Any) {
            entity.id should equal(id2)
            key should equal("col2")
            data should equal(11)
            ok = true
          }
        },
        SyncEntity(Ref("Article", ConstInt(1)),
          "s", SyncRef("Article", "d"), "s.col1 = d.col1")
        )
      ok should equal(true)
    }
    it("Добавление") {
      var ok1 = false
      var ok2 = false
      run(
        new EmptyEntityManager() {
          override def get(id: EntityId) = Some(new Entity(this, id, M("id" -> 1, "col1" -> 2, "col2" -> 11)))

          override def select(select: eql.Select) =
            Seq()

          override def afterUpdate(entity: Entity, key: String, data: Any) {
            key match {
              case "col1" =>
                data should equal(2)
                ok1 = true
              case "col2" =>
                data should equal(16)
                ok2 = true
              case _ => throw new RuntimeException("Error")
            }
          }

          override def insert(description: Description, dataSource: DataSource) =
            new Entity(this, new SqlEntityId(new DataSource(pack, "ds") {
              override def store = new SqlPersistentStore("test"){
                override def insert(insert: Insert, parameters: Map[String, Any]) = 20
              }
            } , description, 0))
        },
        Def("sync", SyncDeclaration(SyncRef("Article", "s"), SyncRef("Article", "d"), "s.col1 = d.col1", Seq(
          Set(Dot(Ref("d"), Ref("col1")), Dot(Ref("s"), Ref("col1"))),
          Set(Dot(Ref("d"), Ref("col2")), Plus(Dot(Ref("s"), Ref("col2")), ConstInt(5)))
        ), false)),
        SyncBy(Ref("Article", ConstInt(1)), Ref("sync")))
      ok1 should equal(true)
      ok2 should equal(true)
    }

    it("ToMany") {
      val id2 = new SqlEntityId(ps, article, 2)
      run(
        new EmptyEntityManager() {
          override def get(id: EntityId) = id.asInstanceOf[SqlEntityId].id match {
            case 1 => Some(new Entity(this, id, M("id" -> 1, "col1" -> 2, "col2" -> 11)))
            case 2 => Some(new Entity(this, id, M("id" -> 2, "col1" -> 2, "col2" -> 11)))
          }

          def id(v : Int) = new SqlEntityId(ps, cons, v)
          def e(m : collection.mutable.Map[String, Any]) = new Entity(this, id(m("id").asInstanceOf[Int]), m)
          override def select(select: eql.Select) = {
            val f = select.from.asInstanceOf[eql.FromEntity]
            f.entity should equal(cons)
            val a = f.alias
            select.where.get match {
              case eql.And(eql.Equal(eql.Ref(Some("d"), "uid"), eql.Const(1)),
              eql.Equal(eql.Ref(Some("d"), "article"), eql.Const(2))) =>
                Seq(e(M("id" -> 3, "article" -> 2, "uid" -> 1, "col1" -> 11)))

              case eql.And(eql.Equal(eql.Ref(Some("d"), "uid"), eql.Const(2)),
              eql.Equal(eql.Ref(Some("d"), "article"), eql.Const(2))) => Seq()

              case _ => throw new RuntimeException("Unknown where %s".format(select.where.get))
            }
          }


          override def lazyLoad(entity: Entity, field : Field, data : Any) = {
            field.name should equal("conses")
            entity.id.asInstanceOf[SqlEntityId].id match {
              case 1 => Seq(
                e(M("id" -> 1, "article" -> 1, "uid" -> 1, "col1" -> 22)),
                e(M("id" -> 2, "article" -> 1, "uid" -> 2, "col1" -> 32)))
              case 2 => Seq(
                e(M("id" -> 3, "article" -> 2, "uid" -> 1, "col1" -> 11)),
                e(M("id" -> 4, "article" -> 2, "uid" -> 3, "col1" -> 16)))
              case _ => throw new RuntimeException("Unknown id")
            }
          }

          override def insert(description: Description, dataSource: DataSource) =
            new Entity(this, new SqlEntityId(new DataSource(pack, "db") {
              override def store = new SqlPersistentStore("test"){
                override def insert(insert: Insert, parameters: Map[String, Any]) = 5
              }
            }, description, 0))
        },
        Val("article1", Ref("Article", ConstInt(1))),
        Val("article2", Ref("Article", ConstInt(2))),
        Set(Dot(Ref("article2"), Ref("conses")),
          SyncEntity(Dot(Ref("article1"), Ref("conses")),
            "s", SyncRef("Cons", "d"), "d.uid = s.uid"
          )),
        Dot(Ref("article2"), Ref("conses"))).asInstanceOf[Seq[Entity]]
    }

  }
}