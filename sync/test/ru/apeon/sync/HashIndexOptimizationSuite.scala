package ru.apeon.sync

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import ru.apeon.core.eql
import eql.{DataSourceExpressionDataSource, DataSourceExpressionDefault}
import ru.apeon.core.entity._
import ru.apeon.core.script.{Def, ScriptTest}

class HashIndexOptimizationSuite extends FunSuite with ShouldMatchers with EntityDefine with ScriptTest{
  test("Hash") {
    withModel{
      desc("A").decl(id, att("uid", int), att("data", int),
        Def("syncWhere", seq("uid"))
      ).b
      model.addDataSource(DataSource(pack, "to"))
      SyncListener.preLoad(model)
    }


    var ok1 = false
    var ok2 = false
    var ok3 = false
    var ok4 = false
    run(
      new TestEntityManager{
        override def select(select: eql.Select) = select match {
          case eql.Select(f : eql.FromEntity, Seq(), None, Seq()) => f.dataSourceExpression match {
            case DataSourceExpressionDefault() =>
              Seq(
                e(des("A"), 1, "uid" -> 1, "data" -> 11),
                e(des("A"), 2, "uid" -> 2, "data" -> 22)
              )
            case DataSourceExpressionDataSource(DataSource(_, "to")) =>
              Seq(
                e(des("A"), 31, "uid" -> 1, "data" -> 66)
              )
          }
          case a => fail("Error %s".format(a))
        }

        override def afterInsert(entity: Entity) {
          ok1 should equal(false)
          ok1 = true
        }
        override def afterUpdate(entity: Entity, key: String, data: Any) {
          entity.id match {
            case OneEntityId(_, _, 31) =>
              key should equal("data")
              data should equal(11)
              ok2 should equal(false)
              ok2 = true
            case t : TemporaryEntityId => key match {
              case "uid" =>
                data should equal(2)
                ok3 should equal(false)
                ok3 = true
              case "data" =>
                data should equal(22)
                ok4 should equal(false)
                ok4 = true
            }
          }
        }
      },
      (Eql("from A") ~ ref("select")) ~ refDs("sync", ref("to"), ref("Sync") ~ ref("HashIndexOptimization"))
    )
    ok1 should equal(true)
    ok2 should equal(true)
    ok3 should equal(true)
    ok4 should equal(true)
  }
}