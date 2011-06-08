package ru.apeon.core.script

import ru.apeon.core.entity._
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import java.util.Calendar

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
    it("toDec") {
      run(Dot("20.567", Ref("toDec"))) should equal (BigDecimal(20.567))
      run(Dot("20.567", Ref("toDec", Some(Seq(Par(1)))))) should equal (BigDecimal(20.5))
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
      round(1231.585, 2) should equal (1231.59)
      round(1231.584, 2) should equal (1231.58)
      round(1231.587, 2) should equal (1231.59)

      run(Dot(ConstDecimal(BigDecimal(1.5)), Ref("round"))) should equal (2)
    }
  }

  describe("Датные функции") {
    it("daysTo") {
      val cal = Calendar.getInstance
      cal.set(2010, 01, 01)
      val start = cal.getTime
      cal.set(2010, 01, 04)
      val end = cal.getTime

      cal.set(2010, 01, 02)
      val m1 = cal.getTime
      cal.set(2010, 01, 03)
      val m2 = cal.getTime

      run(Dot(ConstDate(start), Ref("daysTo", Some(Seq(Par(ConstDate(end)))))))should equal (
        Seq(start, m1, m2, end))
    }
  }

  describe("Коллекции") {
    it("isEmpty") {
      run(Dot(ConstSeq(Seq()), Ref("isEmpty"))) should equal(true)
      run(Dot(ConstSeq(Seq(ConstInt(1))), Ref("isEmpty"))) should equal(false)
    }
    it("size") {
      run(Dot(ConstSeq(Seq(ConstInt(1), ConstInt(2))), Ref("size"))) should equal(2)
    }
  }
}