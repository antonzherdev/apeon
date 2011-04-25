package ru.apeon.core.sql

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import java.util.Date

/**
 * @author Anton Zherdev
 */

class SqlTextGenSuite extends FunSuite with ShouldMatchers {
  test("Simple select") {
    Select(From("test"))
            .toString should be(
      "select *\nfrom\n\t\"test\";")
  }

  test("Select column") {
    Select(From("test"), Seq(
      Column(ConstNumeric(5))))
            .toString should be(
      "select\n\t5\nfrom\n\t\"test\";")
  }

  test("Select some columns") {
    Select(From("ttt"), Seq(
      Column(ConstNumeric(5)),
      Column(ConstNumeric(7))
    )).toString should be(
      "select\n\t5,\n\t7\nfrom\n\t\"ttt\";")
  }

  test("Select column with name") {
    Select(From("test"), Seq(
      Column(ConstNumeric(5), Some("zzz"))))
            .toString should be(
      "select\n\t5 as \"zzz\"\nfrom\n\t\"test\";")
  }

  test("Where") {
    Select(From("test"),
      where = Some(ConstNumeric(5)))
            .toString should be(
      "select *\nfrom\n\t\"test\"\nwhere\n\t5;")
  }

  test("==") {
    Equal(ConstNumeric(5), ConstNumeric(6)).toString should be("5 = 6")
  }
  test("!=") {
    NotEqual(ConstNumeric(5), ConstNumeric(6)).toString should be("5 <> 6")
  }
  test("> >= < <=") {
    More(ConstNumeric(5), ConstNumeric(6)).toString should be("5 > 6")
    MoreOrEqual(ConstNumeric(5), ConstNumeric(6)).toString should be("5 >= 6")
    Less(ConstNumeric(5), ConstNumeric(6)).toString should be("5 < 6")
    LessOrEqual(ConstNumeric(5), ConstNumeric(6)).toString should be("5 <= 6")
  }

  test("Column reference") {
    Ref(FromTable(SqlTable("", "tt"), None), "nn").toString should be("\"tt\".\"nn\"")
    Ref(FromTable(SqlTable("", "tt"), Some("aa")), "nn").toString should be("\"aa\".\"nn\"")
  }

  test("Parameter") {
    Parameter("nn").toString should be(":nn")
  }

  test("Parameter with par") {
    Parameter("nn").toString(Map("nn" -> 10)) should be("10")
  }

  test("Parameter with par list") {
    Equal(ConstNumeric(1), Parameter("nn")).toString( Map("nn" -> List(10, 20))) should be("1 in (10, 20)")
  }

  test("String constant") {
    ConstString("sasa'dss").toString should be("'sasa''dss'")
  }

  test("Like") {
    Like(ConstString("z"), ConstString("z")).toString should be("'z' like 'z'")
  }

  test("AndOr") {
    And(Or(ConstNumeric(1), ConstNumeric(2)), ConstNumeric(3)).toString should be("(1 or 2) and 3")
  }

  test("Cross join") {
    Select(From("test", CrossJoin(From("test2"))))
            .toString should be(
      "select *\nfrom\n\t\"test\"\n\tcross join \"test2\";")
  }

  test("Inner join") {
    Select(From("test", InnerJoin(From("test2"), ConstNumeric(4))))
            .toString should be(
      "select *\nfrom\n\t\"test\"\n\tinner join \"test2\" on 4;")
  }

  test("Inner join2") {
    Select(From("test", InnerJoin(From("test2", InnerJoin(From("test3"), ConstNumeric(5))), ConstNumeric(4))))
            .toString should be(
      "select *\nfrom\n\t\"test\"\n\tinner join \"test2\" on 4\n\tinner join \"test3\" on 5;")
  }

  test("Simple insert") {
    Insert(SqlTable("", "zz"), Seq(InsertColumn("t1", ConstNumeric(5)))).toString should be(
      "insert into \"zz\"(\"t1\")\n" +
              "values(5);")
  }

  test("Insert some columns") {
    Insert(SqlTable("", "zz"), Seq(InsertColumn("t1", ConstNumeric(5)), InsertColumn("t2", ConstNumeric(7)))).toString should be(
      "insert into \"zz\"(\"t1\", \"t2\")\n" +
              "values(5, 7);")
  }

  test("Date") {
    ConstDate(new Date(1265069050765L)).toString should be("'2010-02-02 03:04:10.765'")
  }

  test("Delete") {
    Delete(From("zz")).toString should be(
      "delete from \"zz\";")
  }

  test("Delete with alias") {
    Delete(From("zz", "a")).toString should be(
      "delete from \"zz\"\nfrom \"zz\" as \"a\";")
  }

  test("Delete with where") {
    Delete(From("zz"), Some(ConstNumeric(1))).toString should be(
      "delete from \"zz\"\n" +
              "where\n\t1;")
  }

  test("Update") {
    Update(
      From("zz"),
      Seq(UpdateColumn("t1", ConstNumeric(5)), UpdateColumn("t2", ConstNumeric(7))),
      Some(ConstNumeric(1)) ).toString should be(
      "update \"zz\"\n" +
              "set\n" +
              "\t\"t1\" = 5,\n" +
              "\t\"t2\" = 7\n" +
              "where\n" +
              "\t1;"
      )
  }

  test("Function call") {
    Call("zop", List(ConstNumeric(1), ConstNumeric(2))).toString should be("\"zop\"(1, 2)")
  }

  test("Order by") {
    Select(From("test"), orderBy = Seq(
      OrderBy(ConstNumeric(4)),
      OrderBy(ConstNumeric(5), Desc())))
            .toString should be(
      "select *\nfrom\n\t\"test\"\norder by\n\t4 asc, 5 desc;")
  }

  test("Select statement") {
    ESelect(ConstNumeric(1), From("test"), Some(ConstNumeric(2))).toString should be(
      "(select 1\n" +
              "\tfrom\n" +
              "\t\t\"test\"\n" +
              "\twhere\n" +
              "\t\t2)")
  }

  test("ColumnSeq") {
    Select(From("test"), Seq(ColumnSeq(Seq(Column(ConstNumeric(4), "a"), Column(ConstNumeric(5), "b")), "i"))).
            toString should be(
      "select\n\t\t4 as \"i.a\",\n\t\t5 as \"i.b\"\nfrom\n\t\"test\";")
  }

  test("Not") {
    Not(ConstNumeric(4)).toString should be(
      "not(4)"
    )
  }

  test("exists") {
    Exists(From("test"), Some(ConstNumeric(5))).toString should be(
      "exists(select * from\n\t\t\"test\"\n\twhere\n\t\t5\n)"
    )
  }
}