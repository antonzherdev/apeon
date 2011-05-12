package ru.apeon.core.eql

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import ru.apeon.core.entity._
import ru.apeon.core.script.{Imports, Package, DefaultObjectModel, ScriptDataTypeString}

/**
 * @author Anton Zherdev
 */

class ParserSuite extends FunSuite with ShouldMatchers with EntityDefine{
  val col1 = Attribute(pack, "col1", "", AttributeDataTypeInteger())
  val colToMany = ToManyRef(pack, "test2", "test2", "test1")
  val test1 = desc("test1").decl(Id, col1, colToMany).b

  val colToOne = ToOne(pack, "test1", "id_test1", "test1")
  val test2 = desc("test2").decl(Id, colToOne).b

  val ft1 = FromEntity(test1, None)
  EntityConfiguration.model = model

  fillRef()
  val imports = Some(Imports(pack))

  test("simple from") {
    EqlParser("from test1", model, imports) should equal (Select(From(test1)))
  }

  test("from with datasource") {
    EqlParser("from test1<ds>", model, imports) should equal (Select(FromEntity(test1, None, DataSourceExpressionDataSource(dataSource))))
  }

  test("simple from with alias") {
    EqlParser("from test1 as zzz", model, imports) should equal (Select(From(test1, "zzz")))
  }

  test("simple from with alias and datasource") {
    EqlParser("from test1<ds> as zzz", model, imports) should equal (Select(FromEntity(test1, Some("zzz"), DataSourceExpressionDataSource(dataSource))))
  }

  test("simple select") {
    EqlParser.parseSelect("select 10 as nn from test1", model, imports).columns should equal (
      Seq(Column(ConstNumeric(10), "nn")))
  }

  test("select some columns") {
    EqlParser.parseSelect("select 10 as n, 20 as m from test1", model, imports).columns should equal (
      Seq(
        Column(ConstNumeric(10), "n"),
        Column(ConstNumeric(20), "m")))
  }

  test("expresion column ref") {
    EqlParser.parseSelect("select test1.col1 as nn from test1", model, imports).columns should equal (Seq(
      Column(Dot(From(test1), col1), "nn")
      ))
  }

  test("expresion column ref with alias") {
    EqlParser.parseSelect("select t.col1 as nn from test1 as t", model, imports).columns should equal (Seq(
      Column(Dot(From(test1, "t"), col1), "nn")
      ))
  }

  test("Ref without name") {
    EqlParser.parseSelect("select test1.col1 from test1", model, imports).columns should equal (Seq(
      Column(Dot(From(test1), col1), "col1")
      ))
  }

  test("where") {
    EqlParser.parseSelect("from test1 where 5", model, imports).where.get should equal (ConstNumeric(5))
  }

  test("equal") {
    EqlParser.parseSelect("from test1 where 1= 0", model, imports).where.get should equal(Equal(ConstNumeric(1), ConstNumeric(0)))
  }

  test("not equal") {
    EqlParser.parseSelect("from test1 where 1 != 0", model, imports).where.get should equal(NotEqual(ConstNumeric(1), ConstNumeric(0)))
  }

  test("> >= < <=") {
    EqlParser.parseExpression("1 > 0", model, imports) should equal(More(ConstNumeric(1), ConstNumeric(0)))
    EqlParser.parseExpression("1 >= 0", model, imports) should equal(MoreOrEqual(ConstNumeric(1), ConstNumeric(0)))
    EqlParser.parseExpression("0 < 1", model, imports) should equal(Less(ConstNumeric(0), ConstNumeric(1)))
    EqlParser.parseExpression("0 <= 1", model, imports) should equal(LessOrEqual(ConstNumeric(0), ConstNumeric(1)))
  }

  test("like") {
    EqlParser.parseSelect("from test1 where 1 like 0", model, imports).where.get should equal(Like(ConstNumeric(1), ConstNumeric(0)))
  }

  test("and") {
    EqlParser.parseSelect("from test1 where 1 like 0 and 1=0", model, imports).where.get should equal(
      And(Like(ConstNumeric(1), ConstNumeric(0)), Equal(ConstNumeric(1), ConstNumeric(0)))
      )
  }

  test("or") {
    EqlParser.parseSelect("from test1 where 1=1 or 1 like 0 and 1=0", model, imports).where.get should equal(
      Or(
        Equal(ConstNumeric(1), ConstNumeric(1)),
        And(Like(ConstNumeric(1), ConstNumeric(0)), Equal(ConstNumeric(1), ConstNumeric(0)))
        )
      )
  }

  test("parameter") {
    EqlParser.parseSelect("from test1 where :param", model, imports).where.get should equal (Parameter("param"))
  }

  test("id") {
    EqlParser.parseSelect("from test1 where test1.id", model, imports).where.get should equal (Dot(From(test1), Id))
  }

  test("toOne") {
    EqlParser.parseSelect("from test2 where test2.test1.col1", model, imports).where.get should equal (
      Dot(Dot(From(test2), colToOne), col1))
  }

  test("Simple delete") {
    EqlParser("delete from test1", model, imports) should equal (Delete(From(test1)))
  }

  test("Delete with where") {
    EqlParser("delete from test1 where test1.col1", model, imports) should equal (Delete(From(test1),
      Some(
        Dot(From(test1), col1)
        ) ))
  }

  test("function call") {
    EqlParser.parseSelect("from test1 where test(1, 2) : String", model, imports).where.get should equal (
      SqlFunction("test", Seq(ConstNumeric(1), ConstNumeric(2)), ScriptDataTypeString()))
  }

  test("sum call") {
    EqlParser.parseSelect("from test1 where sum(1)", model, imports).where.get should equal (
      Sum(ConstNumeric(1)))
  }

  test("select expression") {
    EqlParser.parseSelect("from test1 as t where (select test1.col1 from test1 where t.col1)", model, imports).where.get should equal (
      ESelect(Dot(ft1, col1), ft1, Some(Dot(From(test1, "t"), col1))))
  }

  test("select from to many") {
    EqlParser.parseSelect("from test1 as t where (select 1 from t.test2 as z)", model, imports).where.get should equal (
      ESelect(ConstNumeric(1), FromToMany(Dot(FromEntity(test1, Some("t")), colToMany), Some("z"))))
  }

  test("select from to many full ref") {
    EqlParser.parseSelect("from test1.test2 as t", model, imports) should equal (
      Select(FromEntity(test2, Some("t"))))
  }

  test("null") {
    EqlParser.parseSelect("from test1 where null", model, imports).where.get should equal (ConstNull())
  }

  test("brackets") {
    EqlParser.parseSelect("from test1 where (1 or 2) and 3", model, imports).where.get should equal (And(Or(ConstNumeric(1), ConstNumeric(2)), ConstNumeric(3)))
  }

  test("not") {
    EqlParser.parseSelect("from test1 where !1", model, imports).where.get should equal (
      Not(ConstNumeric(1)))
    EqlParser.parseSelect("from test1 where 0 and !1", model, imports).where.get should equal (
      And(ConstNumeric(0), Not(ConstNumeric(1))))
  }

  test("exists") {
    EqlParser.parseSelect("from test1 where exists(test1.test2 as t where 1)", model, imports).where.get should equal (
      Exists(FromToMany(Dot("test1", "test2"), Some("t")), Some(ConstNumeric(1))))
    EqlParser.parseSelect("from test1 where exists(test1.test2)", model, imports).where.get should equal (
      Exists(FromToMany(Dot("test1", "test2"), None), None))
  }

  test("String constant") {
    EqlParser.parseSelect("from test1 where \"dsads\"", model, imports).where.get should equal (
      ConstString("dsads"))
  }

  test("Empty ref") {
    EqlParser.parseExpression("id", model, imports) should equal (
      Ref("id"))
  }

  test("External") {
    case class TestExternal(str : String) extends External {
      def dataType(env: ru.apeon.core.script.Environment) = null
      def eqlExpression = null
    }
    EqlParser.parseSelect("from test1 where %number%", model, imports, Some({s => TestExternal(s)})).where.get should equal (
      TestExternal("number"))
  }

  test("Обращение к колонке to one без указания алиаса") {
    val ref = EqlParser.parseSelect("from test2 where test1.col1", model, imports).where.get.asInstanceOf[Dot]
    ref.left.asInstanceOf[Ref].declaration should equal(colToOne)
  }

  test("Функция с параметрами") {
    EqlParser.parseExpression("test.replace(\"t\", \"z\")", model, imports) should equal (
      Dot(Ref("test"), Ref("replace", ConstString("t"), ConstString("z")))
    )
  }
}