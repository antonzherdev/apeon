package ru.apeon.core.script

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import ru.apeon.core.entity._

class ScriptParserSpec extends Spec with ShouldMatchers with ScriptDefine with EntityDefine {
  def script(statement : Statement*) = new Script(model, pack, statement.toSeq)
  val article = desc("Article").decl(Id).b

  val parser = new ScriptParser(model)
  parser.pack = Some(pack)

  describe("Def") {
    it("Простая функция") {
      parser.parse("def articleSync = {}") should equal (
        script(Def(name = "articleSync", statement = Parentheses()))
      )
    }
  }

  describe("Переменные") {
    it("Val без типа данных") {
      parser.parse("val i = 0") should equal (
        script(Val("i", ConstInt(0)))
      )
    }
    it("Val c типом данных") {
      parser.parse("val i : Int = 3") should equal (
        script(Val("i", ConstInt(3), Some(ScriptDataTypeInteger())))
      )
    }

    it("Var без типа данных") {
      parser.parse("var i = 18") should equal (
        script(Var("i", ConstInt(18)))
      )
    }
    it("Var c типом данных") {
      parser.parse("var i : Int = 3") should equal (
        script(Var("i", ConstInt(3), Some(ScriptDataTypeInteger())))
      )
    }
  }

  describe("Арифметика") {
    it("+") {
      parser.parse("i = 10 + 5") should equal (
        script(Set(Ref("i"), Plus(ConstInt(10), ConstInt(5))))
      )
    }
    it("-") {
      parser.parse("10 - 5") should equal (
        script(Minus(ConstInt(10), ConstInt(5)))
      )
    }

    it("*") {
      parser.parse("10*5") should equal (
        script(Mul(ConstInt(10), ConstInt(5)))
      )
    }
    it("/") {
      parser.parse("10/5") should equal (
        script(Div(ConstInt(10), ConstInt(5)))
      )
    }
  }

  describe("Const") {
    it("Строка") {
      parser.parse("\"test\"") should equal (
        script(ConstString("test"))
      )
    }
    it("Целое") {
      parser.parse("123") should equal (script(ConstInt(123)))
      parser.parse("-123") should equal (script(ConstInt(-123)))
    }
    it("null") {
      parser.parse("null") should equal (
        script(ConstNull())
      )
    }
  }

  describe("Eql") {
    it("Eql statement") {
      parser.parse("`from ToSync as s where s.entityName = 'Invoice'`.select") should equal (
        script(Dot(ConstEql("from ToSync as s where s.entityName = 'Invoice'"), Ref("select")))
      )
    }
  }

  describe("Sync") {
    it("def") {
      parser.parse("def articleSync =\n" +
              "     sync Art as s to Article as a where `a.name = s.name and a.id <> s.id`") should equal (
        script(Def("articleSync", SyncDeclaration(SyncRef("Art", "s"), SyncRef("Article", "a"), "a.name = s.name and a.id <> s.id")))
      )
    }
    it("SyncWithCode") {
      parser.parse("Article<\"db1\">(id) sync as s to Article<\"db2\"> as a\n" +
              "     where `a.uid = s.uid` {\n" +
              "         a.article\n" +
              "     }") should equal (
        script(
          SyncEntity(Ref("Article", Ref("id"), Some(ConstString("db1"))),
            "s", SyncRef("Article", "a", Some(ConstString("db2"))), "a.uid = s.uid", statements = Seq(
              Dot(Ref("a"), Ref("article"))))
        )
      )
    }
    it("SyncBy") {
      parser.parse("a.article sync by articleSync") should equal (
        script(
          SyncBy(Dot(Ref("a"), Ref("article")), Ref("articleSync")))
      )
    }
  }

  describe("Foreach") {
    it("test") {
      parser.parse("`from ToSync as s where s.entityName = 'Invoice'`.select.foreach {ss =>\n" +
              "ss.delete\n" +
              "}") should equal (
        script(
          Dot(Dot(ConstEql("from ToSync as s where s.entityName = 'Invoice'"), Ref("select")),
            ref("foreach", BuiltInFunction(Dot(Ref("ss"), Ref("delete")), Seq("ss")))
          )
        ))
    }
  }

  describe("Dot") {
    it("test") {
      parser.parse("test.column1") should equal (
        script(Dot(Ref("test"), Ref("column1")))
      )
    }

    it("dot.dot") {
      parser.parse("test.column1.id") should equal (
        script(Dot(Dot(Ref("test"), Ref("column1")), Ref("id")))
      )
    }
  }

  describe("Ref") {
    it("test") {
      parser.parse("InvoiceForPayment(ss.entityId)") should equal (
        script(
          Ref("InvoiceForPayment", Dot(Ref("ss"), Ref("entityId"))))
      )
    }

    it("Some parameters") {
      parser.parse("\"%s %s\".format(\"test\", \"test2\")") should equal (
        script(
          Dot("%s %s", Ref("format", Some(Seq(Par("test"), Par("test2"))))))
      )
    }
    it("Empty paramaters") {
      parser.parse("Test()") should equal (
        script(
          Ref("Test", Some(Seq()))
        ))
    }
  }

  describe("=") {
    it("=") {
      parser.parse("test.column1 = ss") should equal (
        script(Set(Dot(Ref("test"), Ref("column1")), Ref("ss")))
      )
    }
    it("+=") {
      parser.parse("test += ss") should equal (script(SetPlus(Ref("test"), Ref("ss"))))
    }
    it("-=") {
      parser.parse("test -= ss") should equal (script(SetMinus(Ref("test"), Ref("ss"))))
    }
    it("*=") {
      parser.parse("test *= ss") should equal (script(SetMul(Ref("test"), Ref("ss"))))
    }
    it("/=") {
      parser.parse("test /= ss") should equal (script(SetDiv(Ref("test"), Ref("ss"))))
    }
  }

  describe("If") {
    it("without Else") {
      parser.parse("if(1) 2") should equal (
        script(If(ConstInt(1), ConstInt(2)))
      )
    }
    it("with Else") {
      parser.parse("if(1) 2 else 3") should equal (
        script(If(ConstInt(1), ConstInt(2), Some(ConstInt(3))))
      )
    }
  }

  describe("Compare") {
    it("==") {
      parser.parse("1 == 1") should equal (script(Equal(ConstInt(1), ConstInt(1))))
    }
    it("!=") {
      parser.parse("1 != 2") should equal (script(NotEqual(ConstInt(1), ConstInt(2))))
    }
    it(">") {
      parser.parse("2 > 1") should equal (script(More(ConstInt(2), ConstInt(1))))
    }
    it(">=") {
      parser.parse("2 >= 1") should equal (script(MoreOrEqual(ConstInt(2), ConstInt(1))))
    }
    it("<") {
      parser.parse("1 < 2") should equal (script(Less(ConstInt(1), ConstInt(2))))
    }
    it("<=") {
      parser.parse("1 <= 2") should equal (script(LessOrEqual(ConstInt(1), ConstInt(2))))
    }
    it("&&") {
      parser.parse("1 == 1 && 2 == 2") should equal (script(
        And(Equal(ConstInt(1), ConstInt(1)), Equal(ConstInt(2), ConstInt(2)))))
    }
    it("||") {
      parser.parse("1 == 1 || 2 == 3") should equal (script(
        Or(Equal(ConstInt(1), ConstInt(1)), Equal(ConstInt(2), ConstInt(3)))))
    }
  }

  describe("Eql constant") {
    it("test") {
      parser.parse("Material(`number = \"20\"`)") should equal (
        script(
          Ref("Material", ConstEql("number = \"20\"")))
      )
    }
  }
}