package ru.apeon.core.entity

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import ru.apeon.core.script._

class InstanceOfSuite extends FunSuite with ShouldMatchers with EntityDefine with ScriptTest{
  test("asInstanceOf") {
    withModel{
      desc("A").decl(id).b
      desc("B").ext("A").decl(att("a", int)).b
    }
    val em = new TestEntityManager{
      val a = e(des("A"), 1)
      val b = e(des("B"), 1, "a" -> 2)
      val a2 = e(des("A"), 2)

      override def get(id: EntityId) = id.description.name match {
        case "A" => id.data match {
          case Seq(1) => Some(a)
          case Seq(2) => Some(a2)
        }
        case "B" => id.data match {
          case Seq(1) => Some(b)
          case Seq(2) => None
        }
      }
    }
    run(em, ref("A", 1) ~ ref("asInstanceOf", ref("B"))) should equal(em.b)
    run(em, ref("A", 1) ~ ref("asInstanceOfOption", ref("B"))) should equal(Some(em.b))
    run(em, ref("A", 2) ~ ref("asInstanceOfOption", ref("B"))) should equal(None)
    run(em, ref("A", 1) ~ ref("isInstanceOf", ref("B"))) should equal(true)
    run(em, ref("A", 2) ~ ref("isInstanceOf", ref("B"))) should equal(false)
  }
}