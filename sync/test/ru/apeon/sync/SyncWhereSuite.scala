package ru.apeon.sync

import org.scalatest.matchers.ShouldMatchers
import collection.mutable.Map
import ru.apeon.core.eql
import ru.apeon.core.script._
import org.scalatest.{Spec}
import ru.apeon.core.entity._

class SyncWhereSuite extends Spec with ShouldMatchers with ScriptTest with EntityDefine{
  SyncListener.preLoad()

  def syncWhere(e : Statement) : Def = Def("syncWhere", e)
  def syncWhere(s : String) : Def = syncWhere(Eql(s))

  describe("Simple") {
    val simpleEm = new TestEntityManager {
      override def get(id: EntityId) = Some(new Entity(this, id, Map("id" -> 1, "uid" -> 2, "uid2" -> 3)))
      override def select(select: eql.Select) = select.where.get match {
        case eql.And(
        eql.Equal(eql.Dot(eql.Ref("d"), eql.Ref("uid")), eql.Const(2)),
        eql.Equal(eql.Dot(eql.Ref("d"), eql.Ref("uid2")), eql.Const(3)))=> Seq()
        case w => fail("Bad where %s".format(w))
      }
    }

    it("Simple sync where") {
      withModel{
        desc("T").decl(id, att("uid", int), att("uid2", int), syncWhere("d.uid = s.uid and d.uid2 = s.uid2")).b
      }
      run(simpleEm,
        ref("T", 1) ~ ref("sync")
      )
    }
    it("Array sync where") {
      withModel{
        desc("T").decl(id, att("uid", int), att("uid2", int), Def("syncWhere", seq("uid", "uid2"))).b
      }
      run(simpleEm,
        ref("T", 1) ~ ref("sync")
      )
    }
  }


  describe("toOne") {
    it("Simple to one") {
      var ok = false
      withModel{
        desc("Cat").decl(id, att("uid", int), syncWhere("d.uid = s.uid")).b
        desc("Doc").decl(id, att("num", int), one("cat", "Cat"), syncWhere("d.num = s.num and d.cat = s.cat")).b
      }
      run(new TestEntityManager {
        override def get(id: EntityId) = Some(new Entity(this, id, Map("id" -> 1, "num" -> 22,
          "cat" -> new Entity(this, new OneEntityId(dataSource, des("Cat"), 3), Map("id" -> 3, "uid" -> 33)))))
        override def select(select: eql.Select) = select.where.get match {
          case eql.And(
          eql.Equal(eql.Dot(eql.Ref("d"), eql.Ref("num")), eql.Const(22)),
          eql.Equal(eql.Dot(eql.Dot(eql.Ref("d"), eql.Ref("cat")), eql.Ref("uid")), eql.Const(33)))
          => Seq()
          case eql.Equal(eql.Dot(eql.Ref("d"), eql.Ref("uid")), eql.Const(33))
          => Seq()
          case w => fail("Bad where %s".format(w))
        }
        override def insert(description : Description, source : DataSource) = {
          if(description.name == "Cat") ok = true
          super.insert(description, source)
        }
      },
        ref("Doc", 1) ~ ref("sync")
      )
      ok should equal(true)
    }
  }

  describe("toMany") {
    it("Simple to many") {
      withModel {
        desc("Doc").decl(id, att("uid", int), many("dets", "Det", "parent"), syncWhere("d.uid = s.uid")).b
        desc("Det").decl(id, one("parent", "Doc"), att("n", int), att("v", int), syncWhere("d.parent = s.parent and d.n = s.n")).b
      }
      var detInserted = 0
      run(new TestEntityManager{
        override def get(id: EntityId) = e(id, "uid" -> 222)
        override def lazyLoad(entity: Entity, field: Field, data: Any) = {
          field.name should equal("dets")
          Seq(
            e(des("Det"), 3, "n" -> 1, "v" -> 40, "parent" -> entity),
            e(des("Det"), 4, "n" -> 2, "v" -> 30, "parent" -> entity)
          )
        }
        override def select(select: eql.Select) = select.where.get match {
          case eql.And(
          eql.Equal(eql.Dot(eql.Dot(eql.Ref("d"), eql.Ref("parent")), eql.Ref("uid")), eql.Const(222)),
          eql.Equal(eql.Dot(eql.Ref("d"), eql.Ref("n")), eql.Const(1))
          )
          => Seq()
          case eql.And(
          eql.Equal(eql.Dot(eql.Dot(eql.Ref("d"), eql.Ref("parent")), eql.Ref("uid")), eql.Const(222)),
          eql.Equal(eql.Dot(eql.Ref("d"), eql.Ref("n")), eql.Const(2))
          )
          => Seq()
          case eql.Equal(eql.Dot(eql.Ref("d"), eql.Ref("uid")), eql.Const(222))
          => Seq()
          case w => fail("Bad where %s".format(w))
        }
        override def insert(description : Description, source : DataSource) = {
          if(description.name == "Det") detInserted += 1
          super.insert(description, source)
        }
      },
        ref("Doc", 1) ~ ref("sync")
      )
      detInserted should equal (2)
    }
  }

  describe("Overriding") {
    def check() {
      run(new TestEntityManager {
        override def get(id: EntityId) = Some(new Entity(this, id, Map("id" -> 1, "uid" -> 2, "uid2" -> 3)))

        override def select(select: eql.Select) = select.where.get match {
          case eql.Equal(eql.Dot(eql.Ref("d"), eql.Ref("uid2")), eql.Const(3)) => Seq()
          case w => fail("Bad where %s".format(w))
        }
      },
        ref("B", 1) ~ ref("sync")
      )
    }
    it("Inherit") {
      withModel{
        desc("A").decl(id, att("uid", int), att("uid2", int), syncWhere("d.uid = s.uid")).b
        desc("B").ext("A").decl(syncWhere("d.uid2 = s.uid2")).b
      }
      check()
    }
    it("ExtendEntity") {
      withModel{
        desc("B").decl(id, att("uid", int), att("uid2", int), syncWhere("d.uid = s.uid")).b
        val env = new DefaultEnvironment(model)
        val e = ExtendEntity(CoreModule, "B", Seq(syncWhere("d.uid2 = s.uid2")))
        e.evaluate(env)
        e.preFillRef(env, Imports(pack))
        e.fillRef(env, Imports(pack))
      }
      check()
    }
  }
}