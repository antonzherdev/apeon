package ru.apeon.sync

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import ru.apeon.core.entity._
import ru.apeon.core.script._

/**
 * @author Anton Zherdev
 */

class SyncProcSpec extends FunSuite with ShouldMatchers with EntityDefine with ScriptTest {
  withModel{
    desc("A").decl(id, att("u", int), att("a", int),
      Def("syncWhere", seq("u")),
      Def("syncProc", (ref("d") ~ ref("a")) := 20, Seq(DefPar("d", ScriptDataTypeEntityByName("A"))))).b
  }
  SyncListener.preLoad(model)

  test("Proc") {
    run(
      new TestEntityManager{
        override def get(id: EntityId) = e(id, "u" -> 11, "a" -> 2)

        override def afterUpdate(entity: Entity, key: String, data: Any) {
          key match {
            case "u" => data should equal (11)
            case "a" => data should equal (20)
          }
        }
      },
      ref("A", 1) ~ ref("sync")
    )
  }
}