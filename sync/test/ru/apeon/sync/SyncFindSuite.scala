package ru.apeon.sync

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import ru.apeon.core.script.{Def, ScriptTest}
import ru.apeon.core.entity.{EntityId, TestEntityManager, EntityDefine}
import ru.apeon.core.eql.Select
import ru.apeon.core.eql

class SyncFindSuite extends FunSuite with ShouldMatchers with EntityDefine with ScriptTest{
  SyncListener.preLoad(model)

  test("Test") {
    withModel {
      desc("A").decl(id, att("uid", int), Def("syncWhere", seq("uid"))).b
    }
    val em = new TestEntityManager {
      val ee = e(des("A"), 3, "uid" -> 2)
      override def get(id: EntityId) = Some(e(id, "uid" -> 2))

      override def select(select: Select) = select.where.get match {
        case eql.Equal(eql.Dot(eql.Ref("d"), eql.Ref("uid")), eql.Const(2)) => Seq(ee)
        case w => fail("Bad where %s".format(w))
      }
    }
    run(em, ref("A", 1) ~ ref("syncFind")) should equal (Some(em.ee))
    run(new TestEntityManager {
      override def get(id: EntityId) = Some(e(id, "uid" -> 2))

      override def select(select: Select) = select.where.get match {
        case eql.Equal(eql.Dot(eql.Ref("d"), eql.Ref("uid")), eql.Const(2)) => Seq()
        case w => fail("Bad where %s".format(w))
      }
    }, ref("A", 1) ~ ref("syncFind")) should equal (None)
  }
}