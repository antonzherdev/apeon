package ru.apeon.core.entity

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSuite}
import ru.apeon.core.script._

/**
 * @author Anton Zherdev
 */

class EntityParserSuite extends FunSuite with ShouldMatchers with EntityDefine {
  val sh = new DefaultObjectModel
  val pack = Package("ru.apeon.test")
  sh.addDataSource(new DataSource(pack, "apeon"))
  FillRef(sh, pack, pack)

  def script(statements : Statement*) = new Script(sh, pack, statements.toSeq)

  test("Empty") {
    val esh = new DefaultObjectModel
    val epack = Package("ru.apeon.test")

    val parsed = ScriptParser.parse(sh, pack,
      """
      entity Article<apeon> {
      }
      """)
    val ed = Description(pack, "Article", "apeon", Table("", "Article"), Seq())
    parsed should equal(script(ed))
    script(ed).evaluate(new DefaultEnvironment(esh))
    esh.entityDescription("ru.apeon.test.Article") should equal(ed)
  }

  test("Наследование") {
    ScriptParser.parse(sh, pack,
      """
      entity Article<apeon> extends Test{
      }
      """)should equal(script(
    Description(pack, "Article", "apeon", Table("", "Article"),Seq(), extendsEntityName = Some("Test")))
    )
  }

  test("Простая колонка") {
    ScriptParser.parse(sh, pack,
      """
      entity Article<apeon> {
        column name(nm) varchar(254)
      }
      """)should equal(script(
    Description(pack, "Article", "apeon", Table("", "Article"),
          Seq(Attribute(pack, "name", "nm", AttributeDataTypeVarchar(254))
          ))
    ))
  }

  test("Колонки dataSource") {
    ScriptParser.parse(sh, pack,
      """
      entity Article<apeon> {
        column name(nm, nam<ds>) varchar(254)
      }
      """)should equal(script(
    Description(pack, "Article", "apeon", Table("", "Article"),
          Seq(Attribute(pack, "name", FieldSources(FieldSource("nm"), Map("ds" -> FieldSource("nam"))), AttributeDataTypeVarchar(254))
          ))
    ))

    ScriptParser.parse(sh, pack,
      """
      entity Article<apeon> {
        column name(nam<ds>) varchar(254)
      }
      """)should equal(script(
    Description(pack, "Article", "apeon", Table("", "Article"),
          Seq(Attribute(pack, "name", FieldSources(FieldSource("name"), Map("ds" -> FieldSource("nam"))), AttributeDataTypeVarchar(254))
          ))
    ))
  }

  test("Ключевое слово, как название колонки") {
    ScriptParser.parse(sh, pack,
      """
      entity Article<apeon> {
        column name("entity") varchar(254)
      }
      """)should equal(script(
    Description(pack, "Article", "apeon", Table("", "Article"),
          Seq(Attribute(pack, "name", "entity", AttributeDataTypeVarchar(254))
          ))
    ))
  }

  test("Простая колонка с таблицей") {
    ScriptParser.parse(sh, pack,
      """
      entity Article<apeon> {
        column name(post.nm) varchar(254)
      }
      """)should equal(script(
    Description(pack, "Article", "apeon", Table("", "Article"),
          Seq(Attribute(pack, "name", ("post", "nm"), AttributeDataTypeVarchar(254))
          ))
    ))
  }

  test("По умолчанию") {
    ScriptParser.parse(sh, pack,
      """
      entity Article<apeon> {
        column name varchar(254) default "test"
      }
      """)should equal(script(
    Description(pack, "Article", "apeon", Table("", "Article"),
          Seq(Attribute(pack, "name", "name", AttributeDataTypeVarchar(254), default = Some(DefaultString("test")))
          ))
    ))
  }

  test("ID") {
    ScriptParser.parse(sh, pack,
      """
      entity Article<apeon> {
        column id integer primary key
      }
      """)should equal(script(
    Description(pack, "Article", "apeon", Table("", "Article"),
          Seq(Attribute(pack, "id", "id", AttributeDataTypeInteger(), isPrimaryKey = true))
        )
    ))
  }

  test("def") {
    ScriptParser.parse(sh, pack,
      """
      entity Article<apeon> {
        def test = 10
      }
      """)should equal(script(
    Description(pack, "Article", "apeon", Table("", "Article"),
          Seq(Def("test", ConstInt(10)))
        )
    ))
  }

  test("Таблица") {
    ScriptParser.parse(sh, pack,
      """
      entity Article<apeon>(dba.inv) {
      }
      """)should equal(script(
    Description(pack, "Article", "apeon", Table("dba", "inv"),Seq())))
  }

  test("Дискриминатор") {
    ScriptParser.parse(sh, pack,
      """
      entity Article<apeon> {
        discriminator is_group="0"
      }
      """)should equal(script(
    Description(pack, "Article", "apeon", Table("", "Article"),Seq(), DiscriminatorColumn("is_group", "0"))))
  }

  test("joined Таблица") {
    ScriptParser.parse(sh, pack,
      """
      entity Article<apeon> {
        table dba.post(id)
      }
      """)should equal(script(
    Description(pack, "Article", "apeon", Table("", "Article"),Seq(), declaredJoinedTables = Seq(JoinedTable(Table("dba", "post"), "id")))))
  }

  test("To One") {
    ScriptParser.parse(sh, pack,
      """
      entity InvoiceForPaymentArticle<apeon>{
        one article Article
      }
      """)should equal(script(
    Description(pack, "InvoiceForPaymentArticle", "apeon", Table("", "InvoiceForPaymentArticle"),
          Seq(ToOne(pack, "article", "article_id", "Article")
          ))
    ))
  }

  test("To Many") {
    ScriptParser.parse(sh, pack,
      """
entity InvoiceForPayment<apeon>{
    many articles InvoiceForPaymentArticle(invoiceForPayment)
}     """)should equal(script(
    Description(pack, "InvoiceForPayment", "apeon", Table("", "InvoiceForPayment"),
          Seq(ToMany(pack, "articles", "InvoiceForPaymentArticle", "invoiceForPayment")
          ))
    ))
  }

  test("Compex") {
    ScriptParser.parse(sh, pack,
      """
entity Article<apeon>(dba.inv) {
    discriminator is_group="0"

    column id integer primary key
    column name(nm) varchar(254)
}
      """)should equal(script(
      Description(pack, "Article", "apeon", Table("dba", "inv"),
        Seq(Attribute(pack, "id", "id", AttributeDataTypeInteger(), isPrimaryKey = true),
          Attribute(pack, "name", "nm", AttributeDataTypeVarchar(254))),
        DiscriminatorColumn("is_group", "0")
      )
    ))
  }

  test("query") {
    val esh = new DefaultObjectModel
    val epack = Package("ru.apeon.test")
    val parsed = ScriptParser.parse(esh, epack,
      """query SYNC {
      def test = 0
      test
      }""")
    val query = Query(esh, epack, "SYNC", Seq(Def("apply", Ref("test")), Def("test", ConstInt(0))))
    parsed should equal(script(query))
    script(query).evaluate(new DefaultEnvironment(esh))
    esh.obj("ru.apeon.test.SYNC") should equal(query)
  }

  test("object") {
    val esh = new DefaultObjectModel
    val epack = Package("ru.apeon.test")
    val parsed = ScriptParser.parse(esh, epack,
      """object Test {
      def apply = 0
      }""")
    val obj = Object(epack, "Test", Seq(Def("apply", ConstInt(0))))
    parsed should equal(script(obj))
    script(obj).evaluate(new DefaultEnvironment(esh))
    esh.obj("ru.apeon.test.Test") should equal(obj)
  }

  test("package") {
    ScriptParser.parse(sh,
      """package ru.apeon.test""")should equal(script(pack))
  }

  test("datasource") {
    ScriptParser.parse(sh, pack,
      """datasource apeon""")should equal(script(DataSource(pack, "apeon")))
  }

  test("extend entity") {
    ScriptParser.parse(sh, pack,
    """extend entity Material{
    column e integer
    }"""
    ) should equal (
      script(ExtendEntity("Material",
        Seq(Attribute(pack, "e", "e", AttributeDataTypeInteger()))
      ))
    )
  }

  test("import") {
    ScriptParser.parse(sh, pack, "import ru.apeon.test1.E1") should equal(script(Import("ru.apeon.test1.E1")))
    ScriptParser.parse(sh, pack, "import ru.apeon.test1._") should equal(script(Import("ru.apeon.test1._")))
  }
}