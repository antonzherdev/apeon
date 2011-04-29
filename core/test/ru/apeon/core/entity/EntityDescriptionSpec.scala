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
      val model = new DefaultObjectModel
      val pack = Package("ru.apeon.core.test")
      val ds = DataSource(pack, "apeon")
      model.addDataSource(ds)
      def att(name : String) = Attribute(pack,name, name, AttributeDataTypeInteger())
      def ent(name : String, columns : Field*) = {
        val r = Description(pack, name, "apeon", Table("a", "a"), columns.toSeq)
        model.addEntityDescription(r)
        val env = new DefaultEnvironment(model)
        r.preFillRef(env, Imports(pack))
        r.fillRef(env, Imports(pack))
        r
      }
      def eent(name : String, ext : String, columns : Field*) = {
        val r = Description(pack, name, "apeon", Table("a", "a"), columns.toSeq, extendsEntityName = Some(ext))
        model.addEntityDescription(r)
        val env = new DefaultEnvironment(model)
        r.preFillRef(env, Imports(pack))
        r.fillRef(env, Imports(pack))
        r
      }

      val eSuperSuper = ent("SuperSuper", att("superSuper1"), att("superSuper2"))
      val eSuper = eent("Super", "SuperSuper", att("super1"), att("super2"))
      val e = eent("E", "Super", att("e1"), att("e2"))

      eSuperSuper.fields should equal(Seq(att("superSuper1"), att("superSuper2")))
      eSuper.fields should equal(Seq(att("super1"), att("super2"), att("superSuper1"), att("superSuper2")))
      e.fields should equal(Seq(att("e1"), att("e2"), att("super1"), att("super2"), att("superSuper1"), att("superSuper2")))
    }
  }

  describe("Расширение сущностей") {
    it("Добавление колонок") {
      val model = new DefaultObjectModel
      val pack = Package("ru.apeon.core.test")
      val ds = DataSource(pack, "apeon")
      model.addDataSource(ds)
      val e = Description(pack, "Test", "apeon", Table("", "test"), Seq(Id))
      model.addEntityDescription(e)

      val t = Attribute(pack, "t", "t", AttributeDataTypeInteger())
      val t2 = Attribute(pack, "t2", "t2", AttributeDataTypeInteger())

      val ee = ExtendEntity("Test", Seq(t, t2))
      FillRef(model, pack, e, ee)
      ee.evaluate(new DefaultEnvironment(model))

      val r = model.entityDescription("ru.apeon.core.test.Test")
      r.fields should equal(
        Seq(Id, t, t2))
    }
  }
}

object FillRef{
  def apply(model : ObjectModel, pack : Package, statements : Statement*) {
    val e = new DefaultEnvironment(model)
    statements.foreach(_.evaluate(e))
    statements.foreach(_.preFillRef(e, Imports(pack)))
    statements.foreach(_.fillRef(e, Imports(pack)))
  }
}