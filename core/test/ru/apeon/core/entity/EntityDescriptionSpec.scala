package ru.apeon.core.entity

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import ru.apeon.core.script._

/**
 * @author Anton Zherdev
 */

class EntityDescriptionSpec  extends Spec with ShouldMatchers with EntityDefine {
  describe("Наследование") {
    it("Добавление колокок"){
      clearModel()
      def att(name : String) = Attribute(pack,name, name, AttributeDataTypeInteger())
      def ent(name : String, columns : Field*) = {
        desc(name).decl(columns : _*).b
      }
      def eent(name : String, ext : String, columns : Field*) = {
        desc(name).decl(columns : _*).ext(ext).b
      }

      val eSuperSuper = ent("SuperSuper", att("superSuper1"), att("superSuper2"))
      val eSuper = eent("Super", "SuperSuper", att("super1"), att("super2"))
      val e = eent("E", "Super", att("e1"), att("e2"))
      fillRef()

      eSuperSuper.fields should equal(Seq(att("superSuper1"), att("superSuper2")))
      eSuper.fields should equal(Seq(att("super1"), att("super2"), att("superSuper1"), att("superSuper2")))
      e.fields should equal(Seq(att("e1"), att("e2"), att("super1"), att("super2"), att("superSuper1"), att("superSuper2")))
    }
  }

  describe("Расширение сущностей") {
    it("Добавление колонок") {
      clearModel()
      desc("Test").decl(Id).b

      val t = Attribute(pack, "t", "t", AttributeDataTypeInteger())
      val t2 = Attribute(pack, "t2", "t2", AttributeDataTypeInteger())

      val ee = ExtendEntity(CoreModule, "Test", Seq(t, t2))

      val env: DefaultEnvironment = new DefaultEnvironment(model)
      ee.evaluate(env)
      ee.preFillRef(env, Imports(pack))
      ee.fillRef(env, Imports(pack))
      fillRef()

      val r = model.entityDescription("ru.apeon.core.Test")
      r.fields should equal(
        Seq(t, t2, id))
    }
  }

  describe("Расширение вложенных сущностей") {
    it("Добавление колонок") {
      val ee = ExtendEntity(CoreModule, "Test.inner", Seq(att("test", int)))
      clearModel()
      desc("Test").decl(Id,
        ToManyBuiltIn(pack, "inner", desc("Test.inner").decl(id).r)
      ).b

      val env: DefaultEnvironment = new DefaultEnvironment(model)
      ee.evaluate(env)
      ee.preFillRef(env, Imports(pack))
      ee.fillRef(env, Imports(pack))
      fillRef()


      val r = model.entityDescription("ru.apeon.core.Test.inner")
      r.fields should equal(
        Seq(att("test", int), id))
    }
  }
}
