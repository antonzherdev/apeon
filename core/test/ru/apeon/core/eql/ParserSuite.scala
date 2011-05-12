package ru.apeon.core.eql

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import ru.apeon.core.entity._
import ru.apeon.core.script.{Imports, Package, DefaultObjectModel, ScriptDataTypeString}

/**
 * @author Anton Zherdev
 */

class ParserSuite extends FunSuite with ShouldMatchers with EntityDefine{
  val sh  = new DefaultObjectModel
  val pack = Package(sh, "ru.apeon.core.test", "1.0.0")

  val col1 = Attribute(pack, "col1", "", AttributeDataTypeInteger())
  val colToMany = ToMany(pack, "test2", "test2", "test1")
  val test1 = Description(pack, "test1", Table("", "test1"), Seq(Id, col1, colToMany))
  sh.addEntityDescription(test1)

  val colToOne = ToOne(pack, "test1", "id_test1", "test1")
  val test2 = Description(pack, "test2", Table("", "test2"), Seq(Id, colToOne))
  sh.addEntityDescription(test2)

  val ds = DataSource(pack, "ds")
  sh.addDataSource(ds)

  val ft1 = FromEntity(test1, None)
  EntityConfiguration.model = sh

  FillRef(sh, pack, pack, test1, test2)
  val imports = Some(Imports(pack))

  test("simple from") {
    EqlParser("from test1", sh, imports) should equal (Select(From(test1)))
  }

  test("from with datasource") {
    EqlParser("from test1<ds>", sh, imports) should equal (Select(FromEntity(test1, None, DataSourceExpressionDataSource(ds))))
  }

  test("simple from with alias") {
    EqlParser("from test1 as zzz", sh, imports) should equal (Select(From(test1, "zzz")))
  }

  test("simple from with alias and datasource") {
    EqlParser("from test1<ds> as zzz", sh, imports) should equal (Select(FromEntity(test1, Some("zzz"), DataSourceExpressionDataSource(ds))))
  }

  test("simple select") {
    EqlParser.parseSelect("select 10 as nn from test1", sh, imports).columns should equal (
      Seq(Column(ConstNumeric(10), "nn")))
  }

  test("select some columns") {
    EqlParser.parseSelect("select 10 as n, 20 as m from test1", sh, imports).columns should equal (
      Seq(
        Column(ConstNumeric(10), "n"),
        Column(ConstNumeric(20), "m")))
  }

  test("expresion column ref") {
    EqlParser.parseSelect("select test1.col1 as nn from test1", sh, imports).columns should equal (Seq(
      Column(Dot(From(test1), col1), "nn")
      ))
  }

  test("expresion column ref with alias") {
    EqlParser.parseSelect("select t.col1 as nn from test1 as t", sh, imports).columns should equal (Seq(
      Column(Dot(From(test1, "t"), col1), "nn")
      ))
  }

  test("Ref without name") {
    EqlParser.parseSelect("select test1.col1 from test1", sh, imports).columns should equal (Seq(
      Column(Dot(From(test1), col1), "col1")
      ))
  }

  test("where") {
    EqlParser.parseSelect("from test1 where 5", sh, imports).where.get should equal (ConstNumeric(5))
  }

  test("equal") {
    EqlParser.parseSelect("from test1 where 1= 0", sh, imports).where.get should equal(Equal(ConstNumeric(1), ConstNumeric(0)))
  }

  test("not equal") {
    EqlParser.parseSelect("from test1 where 1 != 0", sh, imports).where.get should equal(NotEqual(ConstNumeric(1), ConstNumeric(0)))
  }

  test("> >= < <=") {
    EqlParser.parseExpression("1 > 0", sh, imports) should equal(More(ConstNumeric(1), ConstNumeric(0)))
    EqlParser.parseExpression("1 >= 0", sh, imports) should equal(MoreOrEqual(ConstNumeric(1), ConstNumeric(0)))
    EqlParser.parseExpression("0 < 1", sh, imports) should equal(Less(ConstNumeric(0), ConstNumeric(1)))
    EqlParser.parseExpression("0 <= 1", sh, imports) should equal(LessOrEqual(ConstNumeric(0), ConstNumeric(1)))
  }

  test("like") {
    EqlParser.parseSelect("from test1 where 1 like 0", sh, imports).where.get should equal(Like(ConstNumeric(1), ConstNumeric(0)))
  }

  test("and") {
    EqlParser.parseSelect("from test1 where 1 like 0 and 1=0", sh, imports).where.get should equal(
      And(Like(ConstNumeric(1), ConstNumeric(0)), Equal(ConstNumeric(1), ConstNumeric(0)))
      )
  }

  test("or") {
    EqlParser.parseSelect("from test1 where 1=1 or 1 like 0 and 1=0", sh, imports).where.get should equal(
      Or(
        Equal(ConstNumeric(1), ConstNumeric(1)),
        And(Like(ConstNumeric(1), ConstNumeric(0)), Equal(ConstNumeric(1), ConstNumeric(0)))
        )
      )
  }

  test("parameter") {
    EqlParser.parseSelect("from test1 where :param", sh, imports).where.get should equal (Parameter("param"))
  }

  test("id") {
    EqlParser.parseSelect("from test1 where test1.id", sh, imports).where.get should equal (Dot(From(test1), Id))
  }

  test("toOne") {
    EqlParser.parseSelect("from test2 where test2.test1.col1", sh, imports).where.get should equal (
      Dot(Dot(From(test2), colToOne), col1))
  }

  test("Simple delete") {
    EqlParser("delete from test1", sh, imports) should equal (Delete(From(test1)))
  }

  test("Delete with where") {
    EqlParser("delete from test1 where test1.col1", sh, imports) should equal (Delete(From(test1),
      Some(
        Dot(From(test1), col1)
        ) ))
  }

  test("function call") {
    EqlParser.parseSelect("from test1 where test(1, 2) : String", sh, imports).where.get should equal (
      SqlFunction("test", Seq(ConstNumeric(1), ConstNumeric(2)), ScriptDataTypeString()))
  }

  test("sum call") {
    EqlParser.parseSelect("from test1 where sum(1)", sh, imports).where.get should equal (
      Sum(ConstNumeric(1)))
  }

  test("select expression") {
    EqlParser.parseSelect("from test1 as t where (select test1.col1 from test1 where t.col1)", sh, imports).where.get should equal (
      ESelect(Dot(ft1, col1), ft1, Some(Dot(From(test1, "t"), col1))))
  }

  test("select from to many") {
    EqlParser.parseSelect("from test1 where (select 1 from test1.test2 as z)", sh, imports).where.get should equal (
      ESelect(ConstNumeric(1), FromToMany(Dot(ft1, colToMany), Some("z"))))
  }

  test("null") {
    EqlParser.parseSelect("from test1 where null", sh, imports).where.get should equal (ConstNull())
  }

  test("brackets") {
    EqlParser.parseSelect("from test1 where (1 or 2) and 3", sh, imports).where.get should equal (And(Or(ConstNumeric(1), ConstNumeric(2)), ConstNumeric(3)))
  }

  test("not") {
    EqlParser.parseSelect("from test1 where !1", sh, imports).where.get should equal (
      Not(ConstNumeric(1)))
    EqlParser.parseSelect("from test1 where 0 and !1", sh, imports).where.get should equal (
      And(ConstNumeric(0), Not(ConstNumeric(1))))
  }

  test("exists") {
    EqlParser.parseSelect("from test1 where exists(test1.test2 as t where 1)", sh, imports).where.get should equal (
      Exists(FromToMany(Dot("test1", "test2"), Some("t")), Some(ConstNumeric(1))))
    EqlParser.parseSelect("from test1 where exists(test1.test2)", sh, imports).where.get should equal (
      Exists(FromToMany(Dot("test1", "test2"), None), None))
  }

  test("String constant") {
    EqlParser.parseSelect("from test1 where \"dsads\"", sh, imports).where.get should equal (
      ConstString("dsads"))
  }

  test("Empty ref") {
    EqlParser.parseExpression("id", sh, imports) should equal (
      Ref("id"))
  }

  test("External") {
    case class TestExternal(str : String) extends External {
      def dataType(env: ru.apeon.core.script.Environment) = null
      def eqlExpression = null
    }
    EqlParser.parseSelect("from test1 where %number%", sh, imports, Some({s => TestExternal(s)})).where.get should equal (
      TestExternal("number"))
  }

  test("Обращение к колонке to one без указания алиаса") {
    val ref = EqlParser.parseSelect("from test2 where test1.col1", sh, imports).where.get.asInstanceOf[Dot]
    ref.left.asInstanceOf[Ref].declaration should equal(colToOne)
  }
}