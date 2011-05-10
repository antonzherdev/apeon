package ru.apeon.core.eql

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import ru.apeon.core._
import entity._
import script._


/**
 * @author Anton Zherdev
 */

class ParseToSqlSuite extends FunSuite with ShouldMatchers with EntityDefine{
  val entityName = Attribute(pack, "entityName", "entityName", AttributeDataTypeVarchar(64))
  val toSync = desc("ToSync").table("x_sync").decl(Id, entityName).b
  override def createDataSource  = new DataSource(pack, "ds") {
    override def store = EntityConfiguration.store
  }
  fillRef()

  test("Sync") {
    SqlGenerator(EqlParser("from ToSync as s where s.entityName = \"Invoice\"", model, Some(Imports(pack))).asInstanceOf[Select]) should equal(
      sql.Select(sql.From("x_sync", "t"), Seq(
        sql.Column(sql.Ref("t", "id"), "id"),
        sql.Column(sql.Ref("t", "entityName"), "entityName")),
        Some(sql.Equal(sql.Ref("t", "entityName"), sql.ConstString("Invoice")))
      )
    )
  }
}

