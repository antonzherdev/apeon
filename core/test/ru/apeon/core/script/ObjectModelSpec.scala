package ru.apeon.core.script

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import ru.apeon.core.entity._

/**
 * @author Anton Zherdev
 */

class ObjectModelSpec extends Spec with ShouldMatchers with EntityDefine {
  describe("Получение сущности") {
    val model = new DefaultObjectModel
    val pack1 = Package(model, "ru.apeon.core.test1", "1.0")
    val pack2 = Package(model, "ru.apeon.core.test2", "1.0")

    def des(pack : Package, name : String) : Description = {
      val ret = Description(pack, name, Table("", ""), Seq(Id))
      model.addEntityDescription(ret)
      ret
    }

    val e1pack1 = des(pack1, "E1")
    val e1pack2 = des(pack2, "E1")
    val e2pack1 = des(pack1, "E2")
    it("Получение по полному имени") {
      model.entityDescription("ru.apeon.core.test1.E1") should equal(e1pack1)
      model.entityDescription("ru.apeon.core.test2.E1") should equal(e1pack2)
    }

    it("Получение короткому имени из текущего пакета") {
      model.entityDescription("E1", Some(Imports(pack1))) should equal(e1pack1)
      model.entityDescription("E1", Some(Imports(pack2))) should equal(e1pack2)
      model.entityDescription("E2", Some(Imports(pack1))) should equal(e2pack1)
      model.entityDescriptionOption("E2", Some(Imports(pack2))) should equal(None)
    }

    it("Получение по короткому имени из другого пакета по импорту") {
      model.entityDescription("E2", Some(Imports(pack2, Seq("ru.apeon.core.test1.E2")))) should equal(e2pack1)
    }

    it("Получение по короткому имени при импорте всего пакета") {
      val env = Some(Imports(pack2, Seq("ru.apeon.core.test1._")))
      model.entityDescription("E2", env) should equal(e2pack1)
      model.entityDescription("E1", env) should equal(e1pack2)
    }

    it("Получение по имени с частью пакета в имени") {
      val env = Some(Imports(pack1, Seq("ru.apeon.core._")))
      model.entityDescription("test2.E1", env) should equal(e1pack2)
    }
  }
}