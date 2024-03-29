package ru.apeon.core.entity

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSuite}
import ru.apeon.core.script._

/**
 * @author Anton Zherdev
 */

class EntityParserSuite extends FunSuite with ShouldMatchers with EntityDefine {
  fillRef()

  def script(statements : Statement*) = new Script(model, pack, statements.toSeq)

  test("Empty") {
    clearModel()
    val parsed = ScriptParser.parse(model, CoreModule, pack,
      """
      entity Article<ds> {
      }
      """)
    val ed = desc("Article").b
    parsed should equal(script(ed))
    clearModel()
    script(ed).evaluate(new DefaultEnvironment(model))
    model.entityDescription("ru.apeon.core.Article") should equal(ed)
  }

  test("Наследование") {
    ScriptParser.parse(model, CoreModule, pack,
      """
      entity Article<ds> extends Test{
      }
      """)should equal(script(
    desc("Article").ext("Test").b
    ))
  }

  test("Простая колонка") {
    ScriptParser.parse(model, CoreModule, pack,
      """
      entity Article<ds> {
        column name(nm) String(254)
      }
      """)should equal(script(
    desc("Article").decl(Attribute(pack, "name", "nm", AttributeDataTypeString(Some(254)))).b
    ))
  }

  test("Колонки dataSource") {
    ScriptParser.parse(model, CoreModule, pack,
      """
      entity Article<ds> {
        column name(nm, nam<ds>) String(254)
      }
      """)should equal(script(
    desc("Article").decl(
          Attribute(pack, "name", FieldSources(FieldSource("nm"), Map("ds" -> FieldSource("nam"))), AttributeDataTypeString(Some(254)))
          ).b
    ))

    ScriptParser.parse(model, CoreModule, pack,
      """
      entity Article<ds> {
        column name(nam<ds>) String
      }
      """)should equal(script(
    desc("Article").decl(
          Attribute(pack, "name", FieldSources(NullFieldSource(), Map("ds" -> FieldSource("nam"))), AttributeDataTypeString())
          ).b
    ))
  }

  test("Ключевое слово, как название колонки") {
    ScriptParser.parse(model, CoreModule, pack,
      """
      entity Article<ds> {
        column name("entity") String(254)
      }
      """)should equal(script(
    desc("Article").decl(
          Attribute(pack, "name", "entity", AttributeDataTypeString(Some(254)))
          ).b
    ))
  }

  test("Простая колонка с таблицей") {
    ScriptParser.parse(model, CoreModule, pack,
      """
      entity Article<ds> {
        column name(post.nm) String(254)
      }
      """)should equal(script(
    desc("Article").decl(
          Attribute(pack, "name", ("post", "nm"), AttributeDataTypeString(Some(254)))
          ).b
    ))
  }

  test("По умолчанию") {
    ScriptParser.parse(model, CoreModule, pack,
      """
      entity Article<ds> {
        column name String(254) default "test"
      }
      """)should equal(script(
    desc("Article").decl(
          Attribute(pack, "name", "name", AttributeDataTypeString(Some(254)), default = Some(DefaultString("test")))
          ).b
    ))
  }

  test("ID") {
    ScriptParser.parse(model, CoreModule, pack,
      """
      entity Article<ds> {
        column id Int primary key
      }
      """)should equal(script(
    desc("Article").decl(
          Attribute(pack, "id", "id", AttributeDataTypeInteger(), isPrimaryKey = true)
          ).b
    ))
  }

  test("def") {
    ScriptParser.parse(model, CoreModule, pack,
      """
      entity Article<ds> {
        def test = 10
      }
      """)should equal(script(
    desc("Article").decl(
          Def("test", ConstInt(10))
        ).b
    ))
  }

  test("Таблица") {
    ScriptParser.parse(model, CoreModule, pack,
      """
      entity Article<ds> {
        table dba.inv
      }
      """)should equal(script(
    desc("Article").table("dba", "inv").b))
  }

  test("Дискриминатор") {
    ScriptParser.parse(model, CoreModule, pack,
      """
      entity Article<ds> {
        discriminator is_group="0"
      }
      """)should equal(script(
    desc("Article").discriminator("is_group", "0").b))
  }

  test("joined Таблица") {
    ScriptParser.parse(model, CoreModule, pack,
      """
      entity Article<ds> {
        join dba.post(id)
      }
      """)should equal(script(
    desc("Article").join("dba", "post", "id").b))
  }

  test("To One") {
    ScriptParser.parse(model, CoreModule, pack,
      """
      entity InvoiceForPaymentArticle<ds>{
        one article Article
      }
      """)should equal(script(
    desc("InvoiceForPaymentArticle").decl(
          ToOne(pack, "article", "article_id", "Article")
          ).b
    ))
  }

  test("To One pk") {
    ScriptParser.parse(model, CoreModule, pack,
      """
      entity E<ds>{
        one article Article primary key
      }
      """)should equal(script(
    desc("E").decl(
          ToOne(pack, "article", "article_id", "Article", None, true)
          ).b
    ))
  }

  test("To Many") {
    ScriptParser.parse(model, CoreModule, pack,
      """
entity InvoiceForPayment<ds>{
    many articles InvoiceForPaymentArticle.invoiceForPayment
}     """)should equal(script(
    desc("InvoiceForPayment").decl(
          ToManyRef(pack, "articles", "InvoiceForPaymentArticle", "invoiceForPayment")
          ).b
    ))
  }

  test("Inner to many") {
    clearModel()
    val parsed = ScriptParser.parse(model, CoreModule, pack,
      """
      entity Article<ds> {
        many test(tst) {
          column name String(254)
        }
      }
      """)
    val t = desc("Article.test").table("Article_test").decl(
      ToOne(pack, "parent", "tst", "Article"),
      Attribute(pack, "name", "name", AttributeDataTypeString(Some(254)))
    ).b
    val ed = desc("Article").table("Article").decl(
      ToManyBuiltIn(pack, "test",t)
    ).b
    parsed should equal(script(ed))
    script(ed).evaluate(new DefaultEnvironment(model))
    model.entityDescription("ru.apeon.core.Article.test") should equal(t)
  }

  test("Inner to many parent") {
    val parsed = ScriptParser.parse(model, CoreModule, pack,
      """
      entity Article<ds> {
        many test extends T{
          one parent(tst) Article primary key
          column number Int primary key
        }
      }
      """)
    val t = desc("Article.test").table("Article_test").ext("T").decl(
      ToOne(pack, "parent", "tst", "Article", None, true),
      Attribute(pack, "number", "number", AttributeDataTypeInteger(), None, true)
    ).b
    val ed = desc("Article").table("Article").decl(
      ToManyBuiltIn(pack, "test",t)
    ).b
    parsed should equal(script(ed))
  }

  test("Compex") {
    ScriptParser.parse(model, CoreModule, pack,
      """
entity Article<ds>{
    table dba.inv
    discriminator is_group="0"

    column id Int primary key
    column name(nm) String(254)
}
      """)should equal(script(
      desc("Article").table("dba", "inv").decl(
        Attribute(pack, "id", "id", AttributeDataTypeInteger(), isPrimaryKey = true),
          Attribute(pack, "name", "nm", AttributeDataTypeString(Some(254)))).
        discriminator("is_group", "0").b
    ))
  }

  test("query") {
    clearModel()
    val parsed = ScriptParser.parse(model, CoreModule, pack,
      """query SYNC {
      def test = 0
      test
      }""")
    val query = Query(model, CoreModule, pack, "SYNC", Seq(Def("apply", Ref("test")), Def("test", ConstInt(0))))
    parsed should equal(script(query))
    script(query).evaluate(new DefaultEnvironment(model))
    model.obj("ru.apeon.core.SYNC") should equal(query)
  }

  test("object") {
    clearModel()
    val parsed = ScriptParser.parse(model, CoreModule, pack,
      """object Test {
      def apply = 0
      }""")
    val obj = Object(CoreModule, pack, "Test", Seq(Def("apply", ConstInt(0))))
    parsed should equal(script(obj))
    clearModel()
    script(obj).evaluate(new DefaultEnvironment(model))
    model.obj("ru.apeon.core.Test") should equal(obj)
  }

  test("package") {
    ScriptParser.parse(model, CoreModule,
      """package ru.apeon.core""")should equal(script(pack))
  }

  test("datasource") {
    ScriptParser.parse(model, CoreModule, pack,
      """datasource apeon""")should equal(script(DataSource(pack, "apeon")))
  }

  test("extend entity") {
    ScriptParser.parse(model, CoreModule, pack,
      """extend entity Material{
      column e Int
      }"""
    ) should equal (
      script(ExtendEntity(CoreModule, "Material",
        Seq(Attribute(pack, "e", "e", AttributeDataTypeInteger()))
      ))
    )
    ScriptParser.parse(model, CoreModule, pack,
      """extend entity MatMove.materials{
      column e Int
      }"""
    ) should equal (
      script(ExtendEntity(CoreModule, "MatMove.materials",
        Seq(Attribute(pack, "e", "e", AttributeDataTypeInteger()))
      ))
    )
  }


  test("import") {
    ScriptParser.parse(model, CoreModule, pack, "import ru.apeon.test1.E1") should equal(script(Import("ru.apeon.test1.E1")))
    ScriptParser.parse(model, CoreModule, pack, "import ru.apeon.test1._") should equal(script(Import("ru.apeon.test1._")))
  }
}