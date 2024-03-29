package ru.apeon.core.eql

import org.scalatest.matchers.ShouldMatchers
import ru.apeon.core.entity._
import ru.apeon.core._
import org.scalatest.{Spec}
import script._

/**
 * @author Anton Zherdev
 */

class EsqSqlGeneratorSuite extends Spec with ShouldMatchers with EqlDefine {
  override def createDataSource = new DataSource(pack, "ds") {
    override def store = EntityConfiguration.store
  }

  val col1 = Attribute(pack, "col1", "col1", AttributeDataTypeString())
  val col2 = Attribute(pack, "col2", "col2", AttributeDataTypeInteger())
  val test1 = desc("test1").decl(Id, col1, col2).b
  val eft1 = eql.FromEntity(test1, None)
  val ft1 = sql.FromTable(sql.SqlTable("", "test1"), Some("t"))

  val cols1 = Seq(
    sql.Column(sql.Ref(ft1, "col1"), "col1"),
    sql.Column(sql.Ref(ft1, "col2"), "col2"))
  val cols1_1 = Seq(
    sql.Column(sql.Ref(ft1, "id"), "id"),
    sql.Column(sql.Ref(ft1, "col1"), "col1"),
    sql.Column(sql.Ref(ft1, "col2"), "col2"))
  fillRef()

  describe("Simple") {
    it("Simple select") {
      eql.SqlGenerator(
        eql.Select(eft1, Seq(
          eql.Column(eql.Dot(eft1, col1), "col1")))
      ) should be(
        sql.Select(ft1, Seq(
          sql.Column(sql.Ref(ft1, "col1"), "col1")
        )))
    }

    it("Columns select") {
      eql.SqlGenerator(eql.Select(eft1, Seq(
        eql.Column(eql.Dot(eft1, col1), "col1"),
        eql.Column(eql.Dot(eft1, col2), "col2"))
      )) should be(
        sql.Select(ft1, cols1))
    }

    it("Without columns"){
      eql.SqlGenerator(eql.Select(eft1)) should be(
        sql.Select(ft1, cols1_1))
    }

    it("Where") {
      eql.SqlGenerator(eql.Select(eft1, where = Some(eql.ConstNumeric(5))
      ))should be(
        sql.Select(ft1, cols1_1, where = Some(sql.ConstNumeric(5)))
      )
    }
  }

  describe("ToOne") {
    clearModel()
    model.addEntityDescription(test1)

    val colToOne = ToOne(pack, "col3", "id_test1", "test1")
    val test2 = desc("test2").decl(Id, colToOne).b
    val eft2 = eql.FromEntity(test2, None)
    val ft2 = sql.FromTable(sql.SqlTable("", "test2"), Some("t"))
    val ft2_join = sql.FromTable(sql.SqlTable("", "test1"), Some("t0"))

    val cols2 = Seq(sql.ColumnSeq(Seq(
      sql.Column(sql.Ref(ft2_join, "id"), "id"),
      sql.Column(sql.Ref(ft2_join, "col1"), "col1"),
      sql.Column(sql.Ref(ft2_join, "col2"), "col2")), "col3"))
    val colToOne2 = ToOne(pack, "col4", "id_test2", "test2")
    val test3 = desc("test3").decl(Id, colToOne2).b
    val eft3 = eql.FromEntity(test3, None)
    val ft3 = sql.FromTable(sql.SqlTable("", "test3"), Some("t"))
    val ft3_join2 = sql.FromTable(sql.SqlTable("", "test2"), Some("t0"))
    val ft3_join = sql.FromTable(sql.SqlTable("", "test1"), Some("t1"))
    fillRef()
    it("Simple") {
      eql.SqlGenerator(eql.Select(eft2, Seq(
        eql.Column(eql.Dot(eft2, colToOne), "col3")
      ))) should be(
        sql.Select(ft2.setJoin(
          sql.LeftJoin(ft2_join,
            sql.Equal(sql.Ref(ft2, "id_test1"), sql.Ref(ft2_join, "id"))
          )), cols2))
    }

    it("ID") {
      eql.SqlGenerator(eql.Select(eft2, Seq(
        eql.Column(eql.Dot(eql.Dot(eft2, colToOne), "id"), "col3")
      ))) should be(
        sql.Select(ft2, Seq(
          sql.Column(sql.Ref(ft2, "id_test1"), "col3")
        )))
    }

    it("Col"){
      eql.SqlGenerator(eql.Select(eft2, Seq(
        eql.Column(eql.Dot(eql.Dot(eft2, colToOne), "col1"), "col3")
      ))) should be(
        sql.Select(ft2.setJoin(
          sql.LeftJoin(ft2_join,
            sql.Equal(sql.Ref(ft2, "id_test1"), sql.Ref(ft2_join, "id"))
          )), Seq(
          sql.Column(sql.Ref(ft2_join, "col1"), "col3")
        )))
    }

    it("Inner ref") {
      eql.SqlGenerator(eql.Select(eft3, Seq(
        eql.Column(eql.Dot(eql.Dot(eft3, colToOne2), colToOne), "col3"))
      )) should be(sql.Select(
        ft3.setJoin(sql.LeftJoin(
          ft3_join2.setJoin(sql.LeftJoin(
            ft3_join,
            sql.Equal(sql.Ref(ft3_join2, "id_test1"), sql.Ref(ft3_join, "id"))
          )
          ),
          sql.Equal(sql.Ref(ft3, "id_test2"), sql.Ref(ft3_join2, "id"))
        )
        ),
        Seq(sql.ColumnSeq(Seq(
          sql.Column(sql.Ref(ft3_join, "id"), "id"),
          sql.Column(sql.Ref(ft3_join, "col1"), "col1"),
          sql.Column(sql.Ref(ft3_join, "col2"), "col2")
        ), "col3")
        )
      ))
    }
    it("Inner ref-ref") {
      eql.SqlGenerator(eql.Select(eft3, Seq(
        eql.Column(eql.Dot(eql.Dot(eql.Dot(eft3, colToOne2), colToOne), "col1"), "col1"),
        eql.Column(eql.Dot(eql.Dot(eql.Dot(eft3, colToOne2), colToOne), "col2"), "col2")
      ))) should be(sql.Select(
        ft3.setJoin(sql.LeftJoin(
          ft3_join2.setJoin(sql.LeftJoin(
            ft3_join,
            sql.Equal(sql.Ref(ft3_join2, "id_test1"), sql.Ref(ft3_join, "id")))
          ),
          sql.Equal(sql.Ref(ft3, "id_test2"), sql.Ref(ft3_join2, "id")))
        ),
        Seq(
          sql.Column(sql.Ref(ft3_join, "col1"), "col1"),
          sql.Column(sql.Ref(ft3_join, "col2"), "col2")
        )))
    }

    it("Exists") {
      eql.SqlGenerator(eql.Select(eft1, where = Some(
        eql.Exists(eft2, Some(eql.ConstNumeric(5)))))).where.get should be(
        sql.Exists(sql.FromTable(sql.SqlTable("", "test2"), Some("s_t")), Some(sql.ConstNumeric(5))))
    }
  }


  describe("Discriminator value") {
    val sh = new DefaultObjectModel
    sh.addDataSource(dataSource)
    val pack = Package("ru.apeon.core.test")
    val d_test = desc("d_test1").table("test1").decl(Id).discriminator("tp", "1").b
    sh.addEntityDescription(d_test)
    val d_from = sql.From("test1", "t")

    val d_to_one = ToOne(pack, "t1", "id_t1", "d_test1")
    val d_test2 = desc("d_test2").table("test2").decl(Id, d_to_one).discriminator("tp", "1").b
    val d_from2 = sql.From("test2", "t")
    val d_from2_1 = sql.From("test1", "t0")
    fillRef()

    it("Simple") {
      eql.SqlGenerator(eql.Select(eql.From(d_test))) should be (
        sql.Select(d_from, Seq(
          sql.Column(sql.Ref(d_from, "id"), "id")
        ),
          Some(sql.Equal(sql.Ref(d_from, "tp"), sql.ConstString("1")))
        ))
    }

    it("ref") {
      eql.SqlGenerator(eql.Select(eql.From(d_test2))) should be (
        sql.Select(
          d_from2.setJoin(sql.LeftJoin(
            d_from2_1,
            sql.And(
              sql.Equal(sql.Ref(d_from2, "id_t1"), sql.Ref(d_from2_1, "id")),
              sql.Equal(sql.Ref(d_from2_1, "tp"), sql.ConstString("1"))
            ))),
          Seq(
            sql.Column(sql.Ref(d_from2, "id"), "id"),
            sql.ColumnSeq(Seq(sql.Column(sql.Ref(d_from2_1, "id"), "id")), "t1")
          ),
          Some(sql.Equal(sql.Ref(d_from2, "tp"), sql.ConstString("1")))
        )
      )
    }

    it("Insert discriminator") {
      SqlGenerator(eql.Insert(eql.From(d_test),Seq())) should be (
        Seq(
          sql.Insert(sql.SqlTable("", "test1"),
            Seq(sql.InsertColumn("tp", sql.ConstString("1"))))))
    }

    it("Delete discriminator") {
      SqlGenerator(eql.Delete(eql.From(d_test) , Some(eql.ConstNumeric(1)))) should be (
        sql.Delete(sql.From("test1", "t"), Some(
          sql.And(sql.ConstNumeric(1), sql.Equal(sql.Ref(sql.From("test1", "t"), "tp"), sql.ConstString("1"))))))
    }
  }

  describe("Insert") {
    def insert(ins : sql.Insert, table : String) = Seq(ins)
    it("Insert") {
      SqlGenerator(eql.Insert(eql.From(test1),
        Seq(eql.InsertColumn("col1", eql.ConstNumeric(5)), eql.InsertColumn("col2", eql.ConstNumeric(6))))) should be (
        insert(
          sql.Insert(sql.SqlTable("", "test1"),
            Seq(sql.InsertColumn("col1", sql.ConstNumeric(5)), sql.InsertColumn("col2", sql.ConstNumeric(6))))
        , "test1"))
    }

    it("По умолчанию") {
      clearModel()
      val n = Attribute(pack, "n", "n", AttributeDataTypeInteger())
      val d1 = Attribute(pack, "d1", "d1", AttributeDataTypeInteger(), default = Some(DefaultInt(33)))
      val d2 = Attribute(pack, "d2", "d2", AttributeDataTypeInteger(), default = Some(DefaultInt(44)))
      val e = desc("e").decl(Id, n, d1, d2).b
      fillRef()

      val g = SqlGenerator(eql.Insert(eql.From(e), Seq(
        eql.InsertColumn("d1", eql.Const(55))
      )))
      val s = insert(
        sql.Insert(sql.SqlTable("", "e"), Seq(
          sql.InsertColumn("d1", sql.ConstNumeric(55)),
          sql.InsertColumn("d2", sql.ConstNumeric(44))
        )), "e"
      )
      g should equal(s)
    }
  }

  describe("Delete") {
    it("Delete") {
      SqlGenerator(eql.Delete(eql.From(test1))) should be (
        sql.Delete(sql.From("test1", "t"))
      )
    }

    it("Delete where") {
      val sf = sql.From("test1", "t")
      val ef = eql.From(test1)

      SqlGenerator(eql.Delete(ef, Some(eql.Dot(ef, col1)))) should be (
        sql.Delete(sf, Some(sql.Ref(sf, "col1"))))

    }
  }

  describe("ToMany") {
    clearModel()
    val m_test_to_many = ToManyRef(pack, "many", "m_test2", "tst")
    val m_test1 = desc("m_test1").table("test1").decl(Id, m_test_to_many).b
    model.addEntityDescription(m_test1)
    val m_test_to_one = ToOne(pack, "tst", "id_many", "m_test1")
    val m_test2 = desc("m_test2").table("test2").decl(Id, m_test_to_one).b
    model.addEntityDescription(m_test2)
    fillRef()

    val m_eft1 = new FromEntity(m_test1, None)
    val m_sql_ft1 = sql.From("test1", "t")
    val m_sql_ft2 = sql.From("test2", "t")

    it("generateToMany") {
      SqlGenerator.generateToMany(
        eql.Select(m_eft1,
          Seq(
            eql.Column(eql.Dot(m_eft1, m_test_to_many), "many")
          ))) should be(
        Seq(eql.ToManySelect(m_test_to_many,
          sql.Select(
            m_sql_ft2,
            Seq(
              sql.Column(sql.Ref(m_sql_ft2, "id"), "id"),
              sql.Column(sql.Ref(m_sql_ft2, "id_many"), "tst")
            ),
            Some(sql.Equal(sql.Ref(m_sql_ft2, "id_many"), sql.Parameter("id"))))
        )))
    }

    it("Select expression") {
      val m_sql_ft_sub = sql.From("test2", "s_t")

      eql.SqlGenerator(eql.Select(m_eft1, Seq(
        eql.Column(
          eql.ESelect(
            eql.ConstNumeric(1),
            eql.FromToMany(eql.Dot(m_eft1, m_test_to_many), Some("m"))
          ), "test1"))
      )) should be(
        sql.Select(m_sql_ft1, Seq(sql.Column(
          sql.ESelect(
            sql.ConstNumeric(1),
            m_sql_ft_sub,
            Some(sql.Equal(sql.Ref(m_sql_ft1, "id"), sql.Ref(m_sql_ft_sub, "id_many")))
          ),
          Some("test1")
        ))))
    }

    it("Exists") {
      val m_sql_ft_sub = sql.From("test2", "s_t")

      eql.SqlGenerator(eql.Select(m_eft1, Seq(
        eql.Column(
          eql.Exists(
            eql.FromToMany(eql.Dot(m_eft1, m_test_to_many), Some("m"))
          ), "test1"))
      )) should be(
        sql.Select(m_sql_ft1, Seq(sql.Column(
          sql.Exists(
            m_sql_ft_sub,
            Some(sql.Equal(sql.Ref(m_sql_ft1, "id"), sql.Ref(m_sql_ft_sub, "id_many")))
          ),
          Some("test1")
        ))))
    }
  }

  describe("update"){
    it("Simple") {
      SqlGenerator(eql.Update(eql.From(test1),
        Seq(
          eql.UpdateColumn("col1", eql.ConstNumeric(5)),
          eql.UpdateColumn("col2", eql.ConstNumeric(6))))) should be (
        Seq(sql.Update(sql.From("test1"),
          Seq(sql.UpdateColumn("col1", sql.ConstNumeric(5)), sql.UpdateColumn("col2", sql.ConstNumeric(6)))))
      )
    }
  }

  describe("Order by") {
    it("Simple") {
      eql.SqlGenerator(eql.Select(eft1,
        Seq(eql.Column(eql.Dot(eft1, col1), "col1")),
        orderBy = Seq(
          eql.OrderBy(eql.Dot(eft1, col1)),
          eql.OrderBy(eql.Dot(eft1, col2), eql.Desc())
        ))) should be(
        sql.Select(ft1, Seq(sql.Column(sql.Ref(ft1, "col1"), "col1")),
          orderBy = Seq(
            sql.OrderBy(sql.Ref(ft1, "col1")),
            sql.OrderBy(sql.Ref(ft1, "col2"), sql.Desc())
          )))
    }
  }

  describe("Expression") {
    it("Not") {
      eql.SqlGenerator(eql.Select(eft1, where = Some(eql.Not(eql.ConstNumeric(5))))).where.get should be(
        sql.Not(sql.ConstNumeric(5))
      )
    }
    it("+ - * /") {
      eql.SqlGenerator(eql.Select(eft1, where = Some(eql.Plus(eql.ConstNumeric(1), eql.ConstNumeric(2))))).where.get should be(
        sql.Plus(sql.ConstNumeric(1), sql.ConstNumeric(2))
      )
      eql.SqlGenerator(eql.Select(eft1, where = Some(eql.Minus(eql.ConstNumeric(1), eql.ConstNumeric(2))))).where.get should be(
        sql.Minus(sql.ConstNumeric(1), sql.ConstNumeric(2))
      )
      eql.SqlGenerator(eql.Select(eft1, where = Some(eql.Mul(eql.ConstNumeric(1), eql.ConstNumeric(2))))).where.get should be(
        sql.Mul(sql.ConstNumeric(1), sql.ConstNumeric(2))
      )
      eql.SqlGenerator(eql.Select(eft1, where = Some(eql.Div(eql.ConstNumeric(1), eql.ConstNumeric(2))))).where.get should be(
        sql.Div(sql.ConstNumeric(1), sql.ConstNumeric(2))
      )
    }
  }

  describe("Joined Entity") {
    clearModel()

    val a1 = Attribute(pack, "a1", "col1", AttributeDataTypeInteger())
    val a2 = Attribute(pack, "a2", ("m", "col2"), AttributeDataTypeInteger())
    val a3 = Attribute(pack, "a3", ("s", "col3"), AttributeDataTypeInteger())
    val sid = Attribute(pack, "sid", ("s", "id"), AttributeDataTypeInteger(), isPrimaryKey = true)
    val e = desc("E").table("", "m").decl(Id, a1, a2, a3, sid).join("", "s", "ref").b
    fillRef()

    def sel(column : String) = eql.SqlGenerator(eql.Select(eql.From(e), Seq(eql.Column(eql.Ref(column), "a"))))
    def from: sql.FromTable = {
      sql.From("m", "t",
        sql.InnerJoin(sql.From("s", "t0"), sql.Equal(sql.Ref("t0", "ref"), sql.Ref("t", "id"))))
    }
    def f(alias : String): sql.FromSelect = {
      sql.FromSelect(sql.Select(
        from,
        columns = Seq(
          sql.Column(sql.Ref("t", "id"), "id"),
          sql.Column(sql.Ref("t", "col1"), "a1"),
          sql.Column(sql.Ref("t", "col2"), "a2"),
          sql.Column(sql.Ref("t0", "col3"), "a3"),
          sql.Column(sql.Ref("t0", "id"), "sid")
        )
      ), alias)
    }

    it("Колонка без указания таблицы") {
      sel("a1") should equal (
        sql.Select(f("t"), Seq(sql.Column(sql.Ref("t", "a1"), Some("a"))))
      )
    }
    it("Колонка с указанием основноей таблицы") {
      sel("a2") should equal (
        sql.Select(f("t"), Seq(sql.Column(sql.Ref("t", "a2"), Some("a"))))
      )
    }
    it("Колонка с указанием второй таблицы") {
      sel("a3") should equal (
        sql.Select(f("t"), Seq(sql.Column(sql.Ref("t", "a3"), Some("a"))))
      )
    }

    val o1 = ToOne(pack, "o1", "o1", "E")
    val o = desc("O").table("o").decl(Id, o1).b
    fillRef()
    it("Колонка to One") {
      eql.SqlGenerator(eql.Select(eql.From(o), Seq(eql.Column(eql.Dot(eql.Dot("O", "o1"), "a3"),"a")))) should equal (
        sql.Select(
          sql.From("o", "t",
            sql.LeftJoin(f("t0"),
              sql.Equal(sql.Ref("t", "o1"), sql.Ref("t0", "id"))
            )
          ), Seq(sql.Column(sql.Ref("t0", "a3"), "a")))
      )
    }

    it("update") {
      eql.SqlGenerator(eql.Update(eql.From(e),
        Seq(
          eql.UpdateColumn("a1", eql.Const(10)),
          eql.UpdateColumn("a3", eql.Const(15))))) should equal (
        Seq(
          sql.Update(sql.From("m"), Seq(sql.UpdateColumn("col1", sql.ConstNumeric(10))),
            Some(sql.Exists(f("t"), Some(sql.Equal(sql.Ref("m", "id"), sql.Ref("t", "id")))))
          ),
          sql.Update(sql.From("s"), Seq(sql.UpdateColumn("col3", sql.ConstNumeric(15))),
            Some(sql.Exists(f("t"), Some(sql.Equal(sql.Ref("s", "ref"), sql.Ref("t", "id")))))
          )
        ))
    }

    it("update one") {
      eql.SqlGenerator(eql.Update(eql.From(e),
        Seq(
          eql.UpdateColumn("a3", eql.Const(15))))) should equal (
        Seq(
          sql.Update(sql.From("s"), Seq(sql.UpdateColumn("col3", sql.ConstNumeric(15))),
            Some(sql.Exists(f("t"), Some(sql.Equal(sql.Ref("s", "ref"), sql.Ref("t", "id")))))
          )
        ))
    }

    it("Insert") {
      eql.SqlGenerator(eql.Insert(eql.From(e), Seq(
        eql.InsertColumn("a1", eql.Const(5)),
        eql.InsertColumn("a3", eql.Const(15))
      ))) should equal (
        Seq(
          sql.Insert(sql.SqlTable("", "m"), Seq(sql.InsertColumn("col1", sql.ConstNumeric(5)))),
          sql.Insert(sql.SqlTable("", "s"), Seq(
            sql.InsertColumn("ref", sql.Parameter("l_identity")),
            sql.InsertColumn("col3", sql.ConstNumeric(15))))
        )
      )
    }

    it("Insert one") {
      eql.SqlGenerator(eql.Insert(eql.From(e), Seq(
        eql.InsertColumn("a3", eql.Const(15))
      ))) should equal (
        Seq(
          sql.Insert(sql.SqlTable("", "m"), Seq(sql.InsertColumn("id", sql.ConstNull()))),
          sql.Insert(sql.SqlTable("", "s"), Seq(
            sql.InsertColumn("ref", sql.Parameter("l_identity")),
            sql.InsertColumn("col3", sql.ConstNumeric(15))))
        ))
    }
  }

  def zero: sql.Expression = {
    sql.Expression.constant(0)
  }

  describe("Datasources") {
    val ds1 = DataSource(pack, "ds1")
    val ds2 = DataSource(pack, "ds2")
    def initModel(){withModel{
      model.addDataSource(ds1)
      model.addDataSource(ds2)

      val col = Attribute(pack, "col", FieldSources(FieldSource("def"), Map("ds2" -> FieldSource("ds2"))), int)
      val col2 = Attribute(pack, "col2", FieldSources(NullFieldSource(), Map("ds2" -> FieldSource("col2"))), int)
      desc("E").ds("ds1").decl(col, col2).b
    }
    }

    it("Select") {
      initModel()
      eql.SqlGenerator(from("E").where("E" ~ "col")).where.get should equal(sql.Ref("t", "def"))

      eql.SqlGenerator(from("E", ds2).where("E" ~ "col")).where.get should equal(sql.Ref("t", "ds2"))
    }
    it("Null Select") {
      initModel()
      eql.SqlGenerator(from("E", ds2).col("E" ~ "col2", "c")).columns should equal(
        Seq(sql.Column(sql.Ref("t", "col2"), "c")))

      eql.SqlGenerator(from("E").col("E" ~ "col2", "c")).columns should equal(
        Seq(sql.Column(sql.ConstNull(), "c")))
    }
    it("Update") {
      initModel()
      eql.SqlGenerator(update("E", ds2).set("col", 0).set("col2", 0)).head.columns should equal(
        Seq(sql.UpdateColumn("ds2", zero), sql.UpdateColumn("col2", zero)))

      eql.SqlGenerator(update("E").set("col", 0).set("col2", 0)).head.columns should equal(
        Seq(sql.UpdateColumn("def", zero)))

      eql.SqlGenerator(update("E").set("col2", 0)) should equal(
        Seq())
    }
    it("Insert") {
      initModel()
      eql.SqlGenerator(insert("E", ds2).set("col", 0).set("col2", 0)).head.asInstanceOf[sql.Insert].columns should equal(
        Seq(sql.InsertColumn("ds2", zero), sql.InsertColumn("col2", zero)))

      eql.SqlGenerator(insert("E").set("col", 0).set("col2", 0)).head.asInstanceOf[sql.Insert].columns should equal(
        Seq(sql.InsertColumn("def", zero)))
    }
  }

  describe("Функции") {
    it("toInt") {
      eql.SqlGenerator(eql.Select(eql.From(test1), Seq(
        eql.Column(eql.Dot("test1.col1.toInt"), "col1")
      )
      )) should be(
        sql.Select(ft1, Seq(sql.Column(sql.Cast(sql.Ref(ft1, "col1"), "int"), "col1"))
        ))
    }
    it("replace") {
      eql.SqlGenerator(
        eql.Select(eql.From(test1), where =
          Some(eql.Dot(eql.ConstString("test"), eql.Ref("replace", eql.ConstString("t"), eql.ConstString("z"))
          ))
      )).where.get should be(
        sql.Call("replace", Seq(sql.ConstString("test"), sql.ConstString("t"), sql.ConstString("z")))
      )
    }
  }
}