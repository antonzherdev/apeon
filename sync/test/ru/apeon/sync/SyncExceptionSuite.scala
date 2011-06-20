package ru.apeon.sync

import org.scalatest.matchers.ShouldMatchers
import ru.apeon.core.script._
import ru.apeon.core.entity._
import org.scalatest.{FunSuite}
import ru.apeon.core.eql.{FromEntity, Select}

/**
 * @author Anton Zherdev
 */

class SyncExceptionSuite extends FunSuite with ShouldMatchers with ScriptTest with EntityDefine{
  SyncListener.preLoad(model)

  test("Many entities to sync") {
    withModel{
      desc("A").decl(id, att("uid", int), Def("syncWhere", seq("uid"))).b
      desc("B").decl(id, att("uid", int), one("a", "A"), Def("syncWhere", seq("uid"))).b
    }
    val se = intercept[ScriptException] {
      run(new TestEntityManager{
        override def get(id: EntityId) = e(id, "uid" -> 3, "a" -> e(des("A"), 1, "uid" -> 1))

        override def select(select: Select) = select.from.asInstanceOf[FromEntity].entity.name match{
          case "A" => Seq(
            e(des("A"), 1, "uid" -> 3),
            e(des("A"), 2, "uid" -> 3)
          )
          case "B" => Seq(e(des("B"), 2, "uid" -> 3, "a" -> null))
        }

      },
        ref("B", 1) ~ ref("sync")
      )
    }
    se.printStackTrace()
    se.cause.get.asInstanceOf[SyncException].path should equal(Seq("a"))
  }
}