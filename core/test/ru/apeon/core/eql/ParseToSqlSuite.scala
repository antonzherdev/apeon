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
  val sh = new DefaultObjectModel
  val pack = Package(sh, "ru.apeon.core.test", "1.0.0")
  val entityName = Attribute(pack, "entityName", "entityName", AttributeDataTypeVarchar(64))
  val toSync = Description(pack, "ToSync", Table("", "x_sync"), Seq(Id, entityName))
  sh.addEntityDescription(toSync)
  val ps = new DataSource(pack, "apeon") {
    override def store = EntityConfiguration.store
  }
  sh.addDataSource(ps)
  EntityConfiguration.model = sh
  FillRef(sh, pack, pack, toSync)

  test("Sync") {
    SqlGenerator(EqlParser("from ToSync as s where s.entityName = \"Invoice\"", sh, Some(Imports(pack))).asInstanceOf[Select]) should equal(
      sql.Select(sql.From("x_sync", "t"), Seq(
        sql.Column(sql.Ref("t", "id"), "id"),
        sql.Column(sql.Ref("t", "entityName"), "entityName")),
        Some(sql.Equal(sql.Ref("t", "entityName"), sql.ConstString("Invoice")))
      )
    )
  }
}

