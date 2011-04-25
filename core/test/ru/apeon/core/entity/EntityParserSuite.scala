package ru.apeon.core.entity

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSuite}
import ru.apeon.core.script._

/**
 * @author Anton Zherdev
 */

class EntityParserSuite extends FunSuite with ShouldMatchers with EntityDefine {
  val sh = new DefaultObjectModel
  val pack = new Package(sh, Seq("ru", "apeon", "test"), Some(Seq(1, 0, 0)), Some("apeon"))
  sh.addDataSource(new DataSource(pack, "apeon"))
  sh.addPackage(pack)
  pack.preFillRef(sh, Imports(pack))
  pack.fillRef(new DefaultEnvironment, Imports(pack))

  def script(statements : Statement*) = new Script(pack, statements.toSeq)

  test("Empty") {
    val esh = new DefaultObjectModel
    val epack = Package(sh, "ru.apeon.test", "1.0.0")

    val parsed = ScriptParser.parse(pack,
      """
      entity Article {
      }
      """)
    val ed = Description(pack, "Article", Table("", "Article"), Seq())
    parsed should equal(script(ed))
    script(ed).evaluate(new DefaultEnvironment(esh))
    esh.entityDescription("ru.apeon.test.Article") should equal(ed)
  }

  test("Наследование") {
    ScriptParser.parse(pack,
      """
      entity Article extends Test{
      }
      """)should equal(script(
    Description(pack, "Article", Table("", "Article"),Seq(), extendsEntityName = Some("Test")))
    )
  }

  test("Простая колонка") {
    ScriptParser.parse(pack,
      """
      entity Article {
        column name(nm) varchar(254)
      }
      """)should equal(script(
    Description(pack, "Article", Table("", "Article"),
          Seq(Attribute(pack, "name", "nm", AttributeDataTypeVarchar(254))
          ))
    ))
  }

  test("Колонки dataSource") {
    ScriptParser.parse(pack,
      """
      entity Article {
        column name(nm, nam<ds>) varchar(254)
      }
      """)should equal(script(
    Description(pack, "Article", Table("", "Article"),
          Seq(Attribute(pack, "name", FieldSources(FieldSource("nm"), Map("ds" -> FieldSource("nam"))), AttributeDataTypeVarchar(254))
          ))
    ))

    ScriptParser.parse(pack,
      """
      entity Article {
        column name(nam<ds>) varchar(254)
      }
      """)should equal(script(
    Description(pack, "Article", Table("", "Article"),
          Seq(Attribute(pack, "name", FieldSources(FieldSource("name"), Map("ds" -> FieldSource("nam"))), AttributeDataTypeVarchar(254))
          ))
    ))
  }

  test("Ключевое слово, как название колонки") {
    ScriptParser.parse(pack,
      """
      entity Article {
        column name("entity") varchar(254)
      }
      """)should equal(script(
    Description(pack, "Article", Table("", "Article"),
          Seq(Attribute(pack, "name", "entity", AttributeDataTypeVarchar(254))
          ))
    ))
  }

  test("Простая колонка с таблицей") {
    ScriptParser.parse(pack,
      """
      entity Article {
        column name(post.nm) varchar(254)
      }
      """)should equal(script(
    Description(pack, "Article", Table("", "Article"),
          Seq(Attribute(pack, "name", ("post", "nm"), AttributeDataTypeVarchar(254))
          ))
    ))
  }

  test("По умолчанию") {
    ScriptParser.parse(pack,
      """
      entity Article {
        column name varchar(254) default "test"
      }
      """)should equal(script(
    Description(pack, "Article", Table("", "Article"),
          Seq(Attribute(pack, "name", "name", AttributeDataTypeVarchar(254), default = Some(DefaultString("test")))
          ))
    ))
  }

  test("ID") {
    ScriptParser.parse(pack,
      """
      entity Article {
        column id integer primary key
      }
      """)should equal(script(
    Description(pack, "Article", Table("", "Article"),
          Seq(Attribute(pack, "id", "id", AttributeDataTypeInteger(), isPrimaryKey = true))
        )
    ))
  }

  test("def") {
    ScriptParser.parse(pack,
      """
      entity Article {
        def test = 10
      }
      """)should equal(script(
    Description(pack, "Article", Table("", "Article"),
          Seq(Def("test", ConstInt(10)))
        )
    ))
  }

  test("Таблица") {
    ScriptParser.parse(pack,
      """
      entity Article(dba.inv) {
      }
      """)should equal(script(
    Description(pack, "Article", Table("dba", "inv"),Seq())))
  }

  test("Дискриминатор") {
    ScriptParser.parse(pack,
      """
      entity Article {
        discriminator is_group="0"
      }
      """)should equal(script(
    Description(pack, "Article", Table("", "Article"),Seq(), DiscriminatorColumn("is_group", "0"))))
  }

  test("joined Таблица") {
    ScriptParser.parse(pack,
      """
      entity Article {
        table dba.post(id)
      }
      """)should equal(script(
    Description(pack, "Article", Table("", "Article"),Seq(), declaredJoinedTables = Seq(JoinedTable(Table("dba", "post"), "id")))))
  }

  test("To One") {
    ScriptParser.parse(pack,
      """
      entity InvoiceForPaymentArticle{
        one article Article
      }
      """)should equal(script(
    Description(pack, "InvoiceForPaymentArticle", Table("", "InvoiceForPaymentArticle"),
          Seq(ToOne(pack, "article", "article_id", "Article")
          ))
    ))
  }

  test("To Many") {
    ScriptParser.parse(pack,
      """
entity InvoiceForPayment{
    many articles InvoiceForPaymentArticle(invoiceForPayment)
}     """)should equal(script(
    Description(pack, "InvoiceForPayment", Table("", "InvoiceForPayment"),
          Seq(ToMany(pack, "articles", "InvoiceForPaymentArticle", "invoiceForPayment")
          ))
    ))
  }

  test("Compex") {
    ScriptParser.parse(pack,
      """
entity Article(dba.inv) {
    discriminator is_group="0"

    column id integer primary key
    column name(nm) varchar(254)
}
      """)should equal(script(
      Description(pack, "Article", Table("dba", "inv"),
        Seq(Attribute(pack, "id", "id", AttributeDataTypeInteger(), isPrimaryKey = true),
          Attribute(pack, "name", "nm", AttributeDataTypeVarchar(254))),
        DiscriminatorColumn("is_group", "0")
      )
    ))
  }

  test("query") {
    val esh = new DefaultObjectModel
    val epack = Package(sh, "ru.apeon.test", "1.0.0")
    val parsed = ScriptParser.parse(epack,
      """query SYNC {
      def test = 0
      test
      }""")
    val query = Query(epack, "SYNC", Seq(Def("apply", Ref("test")), Def("test", ConstInt(0))))
    parsed should equal(script(query))
    script(query).evaluate(new DefaultEnvironment(esh))
    esh.obj("ru.apeon.test.SYNC") should equal(query)
  }

  test("object") {
    val esh = new DefaultObjectModel
    val epack = Package(sh, "ru.apeon.test", "1.0.0")
    val parsed = ScriptParser.parse(epack,
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
      """package ru.apeon.test(1.0.0) {
      default datasource apeon
      }""")should equal(script(pack))
  }

  test("datasource") {
    ScriptParser.parse(pack,
      """datasource apeon""")should equal(script(DataSource(pack, "apeon")))
  }

  test("extend entity") {
    ScriptParser.parse(pack,
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
    ScriptParser.parse(pack, "import ru.apeon.test1.E1") should equal(script(Import("ru.apeon.test1.E1")))
    ScriptParser.parse(pack, "import ru.apeon.test1._") should equal(script(Import("ru.apeon.test1._")))
  }
}