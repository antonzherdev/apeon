package ru.apeon.core.script

import ru.apeon.core.entity._
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

/**
 * @author Anton Zherdev
 */

class FunctionSpec extends Spec with ShouldMatchers with EntityDefine with ScriptDefine{
  val sh = new DefaultObjectModel
  val pack = Package(sh, "ru.apeon.core.test", "1.0.0")
  val ds = new DataSource(pack, "ds")
  sh.addDataSource(ds)
  sh.addPackage(pack)
  FillRef(sh, pack, pack)
  def run(statement : Statement*) = Script(pack, statement : _*).evaluate()

  describe("Строковые функции") {
    it("format") {
      run(Dot("test %s %s", Ref("format", Some(Seq(Par("test1"), Par("test2"))) ))) should equal ("test test1 test2")
    }
    it("toInt") {
      run(Dot("20", Ref("toInt"))) should equal (20)
    }
    it("replace") {
      run(Dot("abcba", Ref("replace", Some(Seq(Par("b"), Par("d"))) ))) should equal ("adcda")
    }
  }

  describe("Числовые функции") {
    it("round") {
      def round(value: Double, par: Int): Any = {
        run(Dot(ConstDecimal(BigDecimal(value)), Ref("round", Some(Seq(Par(ConstInt(par)))))))
      }
      round(1.585, 2) should equal (1.59)
      round(1.584, 2) should equal (1.58)
      round(1.587, 2) should equal (1.59)

      run(Dot(ConstDecimal(BigDecimal(1.5)), Ref("round"))) should equal (2)
    }
  }
}