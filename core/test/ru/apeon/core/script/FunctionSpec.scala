package ru.apeon.core.script

import ru.apeon.core.entity._
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import java.util.{Calendar}

/**
 * @author Anton Zherdev
 */

class FunctionSpec extends Spec with ShouldMatchers with EntityDefine with ScriptDefine{
  fillRef()
  def run(statement : Statement*) = Script(model, pack, statement : _*).evaluate()

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
    it("sub") {
      run(ConstString("abcba") ~ ref("sub", 2)) should equal ("cba")
      run(ConstString("abcba") ~ ref("sub", 2, 4)) should equal ("cb")
    }
    it("pos") {
      run(ConstString("abcba") ~ ref("pos", "b")) should equal (1)
      run(ConstString("abcba") ~ ref("pos", "b", 2)) should equal (3)
    }
    it("toDate") {
      val cal = Calendar.getInstance
      cal.clear()
      cal.set(2011, 0, 1, 0, 0, 0)
      run(ConstString("Jan 1, 2011") ~ ref("toDate", "MMM d, yyyy")) should equal (cal.getTime)
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
    it("between") {
      run(ConstInt(2) ~ ref("between", 1, 3)) should equal (true)
      run(ConstInt(1) ~ ref("between", 1, 3)) should equal (true)
      run(ConstInt(3) ~ ref("between", 1, 3)) should equal (true)
      run(ConstInt(0) ~ ref("between", 1, 3)) should equal (false)
      run(ConstInt(4) ~ ref("between", 1, 3)) should equal (false)
      run(ConstInt(2) ~ ref("between", 2, 2)) should equal (true)
    }
    it("in") {
      run(ConstInt(2) ~ ref("in", 3, 2, 1)) should equal (true)
      run(ConstInt(4) ~ ref("in", 3, 2, 1)) should equal (false)
    }
    it("to") {
      run(ConstInt(1) ~ ref("to", 5)) should equal(Seq(1, 2, 3, 4, 5))
      run(ConstInt(1) ~ ref("to", 5, 2)) should equal(Seq(1, 3, 5))
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
    it("diffDays") {
      val cal = Calendar.getInstance
      cal.set(2011, 0, 1)
      val start = cal.getTime
      cal.set(2011, 0, 31)
      val end = cal.getTime

      run(ConstDate(start) ~ ref("diffDays", end)) should equal (30)
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
    it("groupBy") {
      run(seq("CC", "A", "AA", "C", "BB") ~ ref("groupBy", bf(
        ref("_") ~ ref("length")
      ))) should equal (Map( 1 -> Seq("A", "C"), 2 -> Seq("CC", "AA", "BB")))
    }
    it("mapBy") {
      run(seq("CCC", "A", "AA") ~ ref("mapBy", bf(
        ref("_") ~ ref("length")
      ))) should equal (Map( 1 -> "A", 2 -> "AA", 3 -> "CCC"))
    }
    it("map") {
      run(seq("A", "BBB", "CC", "DD") ~ ref("map", bf(
        ref("_") ~ ref("length")
      ))) should equal(Seq(1, 3, 2, 2))
    }
    it("positioned") {
      val s = seq(2, 3, 1)
      run(s ~ ref("apply", 1)) should equal(3)
      run(s ~ ref("head")) should equal(2)
      run(s ~ ref("last")) should equal(1)
      run(s ~ ref("headOption") ~ ref("get")) should equal(2)
      run(s ~ ref("lastOption") ~ ref("get")) should equal(1)
      run(s ~ ref("tail")) should equal(Seq(3, 1))
      run(seq() ~ ref("headOption")) should equal(None)
      run(seq() ~ ref("lastOption")) should equal(None)
    }
    it("sortBy") {
      run(seq(2, 3, 1) ~ ref("sortBy", bf(ref("_")))) should equal(Seq(1, 2, 3))
    }
  }

  describe("Option") {
    it("map") {
      run(seq() ~ ref("headOption") ~ ref("map", bf(ref("_") + 5))) should equal(None)
      run(seq(3) ~ ref("headOption") ~ ref("map", bf(ref("_") + 5))) should equal(Some(8))
    }
  }

  describe("Карты") {
    it("toMap") {
      run(seq(MapItem(1, "A"), MapItem(2, "B")) ~ ref("toMap")) should equal (Map(1 -> "A", 2 -> "B"))
    }
    it("get") {
      run(seq(MapItem(1, "A"), MapItem(2, "B")) ~ ref("toMap") ~ ref("get", 1)) should equal (Some("A"))
      run(seq(MapItem(1, "A"), MapItem(2, "B")) ~ ref("toMap") ~ ref("get", 3)) should equal (None)
    }
    it("apply") {
      run(seq(MapItem(1, "A"), MapItem(2, "B")) ~ ref("toMap") ~ ref("apply", 1)) should equal ("A")
    }
    it("getOrElse") {
      run(seq(MapItem(1, "A"), MapItem(2, "B")) ~ ref("toMap") ~ ref("getOrElse", 1, bf(ConstString("No")))) should equal ("A")
      run(seq(MapItem(1, "A"), MapItem(2, "B")) ~ ref("toMap") ~ ref("getOrElse", 3, bf(ConstString("No")))) should equal ("No")
    }
    it("update") {
      run(seq(MapItem(1, "A"), MapItem(2, "B")) ~ ref("toMap") ~ ref("update", 3, "C")) should equal (
        Map(1 -> "A", 2 -> "B", 3 -> "C")
      )
    }
    it("getOrElseUpdate") {
      run(
        Val("m", seq(MapItem(1, "A"), MapItem(2, "B")) ~ ref("toMap")),
        ref("m") ~ ref("getOrElseUpdate", 1, bf(ConstString("No"))),
        ref("m") ~ ref("getOrElseUpdate", 3, bf(ConstString("No"))),
        ref("m")
      ) should equal (Map(1 -> "A", 2 -> "B", 3 -> "No"))
    }
  }
}