package ru.apeon.sync

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import ru.apeon.core.script._
import ru.apeon.core.entity.{Entity, EntityId, TestEntityManager, EntityDefine}

class AutoSyncAutoTurnOffSuite extends FunSuite with ShouldMatchers with EntityDefine with ScriptTest{
   withModel{
      desc("A").decl(id, att("a1", int), att("a2", int), att("a3", int), Def("syncWhere", seq("a1"))).b
    }
  SyncListener.preLoad(model)
  test("AutoTurnOff") {
    run(
      new TestEntityManager{
        override def get(id: EntityId) = e(id, "a1" -> 11, "a2" -> 2, "a3" -> 4)

        override def afterUpdate(entity: Entity, key: String, data: Any) {
          key match {
            case "a1" => data should equal (11)
            case "a2" => data should equal (20)
            case "a3" => data should equal (4)
          }
        }
      },
      ref("A", 1) ~ ref("sync", bf(
        (ref("d") ~ ref("a2")) := 20
      ))
    )
  }

  test("Skip") {
    run(
      new TestEntityManager{
        override def get(id: EntityId) = e(id, "a1" -> 11, "a2" -> 2, "a3" -> 4)

        override def afterUpdate(entity: Entity, key: String, data: Any) {
          key match {
            case "a1" => data should equal (11)
            case "a3" => data should equal (4)
          }
        }
      },
      ref("A", 1) ~ ref("sync", bf(
        ref("syncSkip", "a2")
      ))
    )
  }
}