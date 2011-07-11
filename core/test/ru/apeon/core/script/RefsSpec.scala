package ru.apeon.core.script

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import ru.apeon.core.entity._

/**
 * @author Anton Zherdev
 */

class RefsSpec extends Spec with ShouldMatchers with ScriptDefine {
  describe("Ссылки на локальные декларации") {
    val model = new DefaultObjectModel
    val pack = Package( "ru.apeon.core.test")
    val ds = new DataSource(pack, "ds")
    model.addDataSource(ds)

    def run(st : Statement*) = Script(model, pack, st : _*).evaluate()

    it("val") {
      run(
        Val("a", ConstInt(234)),
        Ref("a")
      ) should equal (234)
    }

    it("Simple") {
      run(
        Def("a", ConstInt(234)),
        Ref("a")
      ) should equal (234)
    }

    it("Parameter") {
      run(
        Def("a", Plus(Ref("i"), ConstInt(10)), Seq(DefPar("i", ScriptDataTypeInteger()))),
        Ref("a", ConstInt(20))
      ) should equal (30)
    }
  }


  describe("Ссылки внутри this") {
    it("Ссылка должна проставляться на функцию внутри объекта, а не на функцию другого объекта") {
      val model = new DefaultObjectModel
      val pack = Package("ru.apeon.core.test")
      val ds = new DataSource(pack, "ds")
      model.addDataSource(ds)
      val a = Query(model, CoreModule, pack, "A", Seq(Def("f", "A"), Def("apply", Ref("f"))))
      val b = Query(model, CoreModule, pack, "B", Seq(Def("apply", Ref("f")), Def("f", "B")))
      Script(model, pack, a, b).evaluate()
      model.obj("ru.apeon.core.test.A").asInstanceOf[Query].execute() should equal("A")
      model.obj("ru.apeon.core.test.B").asInstanceOf[Query].execute() should equal("B")
    }
  }

}