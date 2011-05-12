package ru.apeon.core.entity

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import collection.Map
import ru.apeon.core.eql._
import ru.apeon.core.script.{Package}

/**
 * @author Anton Zherdev
 */

class EntityManagerSpec extends Spec with ShouldMatchers with EntityDefine {
  override def createDataSource = new DataSource(pack, "ds") {
    override def store = EntityConfiguration.store
  }
  var store2 : PersistentStore = _

  val ds2 = new DataSource(pack, "ds2") {
    override def store = store2
  }
  model.addDataSource(ds2)

  val col1 = Attribute(pack, "col1", "col1", AttributeDataTypeInteger())
  val col2 = Attribute(pack, "col2", "col2", AttributeDataTypeInteger(), default = Some(DefaultInt(20)))
  val invoicesCol = ToManyRef(pack, "invoices", "Invoice", "article")
  val article = desc("Article").decl(Id, col1, col2, invoicesCol).b

  val articleCol = ToOne(pack, "article", "id_article", "Article")
  val invoice = desc("Invoice").decl(Id, articleCol, col1).b

  fillRef()

  val M = collection.mutable.Map
  EntityConfiguration.model = model

  def nem = new DefaultEntityManager

  abstract class PS extends PersistentStore {
    def name = "test"

    def update(update: Update, parameters: Map[String, Any]) {
      throw new RuntimeException("Unover")
    }
    def select(select: Select, parameters: Map[String, Any]) : Seq[collection.mutable.Map[String, Any]] = throw new RuntimeException("Unover")
    def insert(insert: Insert, parameters: Map[String, Any]) : Int = throw new RuntimeException("Unover")
    def delete(delete: Delete, parameters: Map[String, Any]) {
      throw new RuntimeException("Unover")
    }

    def rollback() {}
    def commit() {}
    def beginTransaction() {}
  }

  describe("Считование сущностей") {
    EntityConfiguration.store = new PS{
      def m1 = collection.mutable.Map[String, Any]("id" -> 1, "col1" -> 2, "col2" -> 0)
      def m2 = collection.mutable.Map[String, Any]("id" -> 2, "col1" -> 3, "col2" -> 0)

      override def select (select: Select, parameters: Map[String, Any]) = select.where match {
        case None => Seq(m1, m2)
        case Some(Equal(Ref("id"), Const(1))) => Seq(m1)
        case _ => Seq()
      }
    }
    it("Сущности должны считаться") {
      val entities = nem.select(Select(From(article)))
      entities.size should equal (2)
      val e1 = entities.head
      e1.id should equal(new OneEntityId(dataSource, article, 1))
      e1("col1") should equal (2)
      val e2 = entities.tail.head
      e2.id should equal(new OneEntityId(dataSource, article, 2))
      e2("col1") should equal (3)
    }
    it("Сущности должна получасться по идентификатору") {
      val id = new OneEntityId(dataSource, article, 1)
      val e = nem.get(id).get
      e.id should equal(id)
      e("col1") should equal (2)
    }
    it("Считование должно учитывать измения") {
      val em = nem
      val id = new OneEntityId(dataSource, article, 1)
      val e = em.get(id).get
      e("col1") should equal (2)
      e.update("col1", 33)
      e("col1") should equal (33)

      val _e = em.get(id).get
      _e("col1") should equal (33)

      val entities = em.select(Select(From(article)))
      entities.size should equal (2)
      val e1 = entities.head
      e1.id should equal(new OneEntityId(dataSource, article, 1))
      e1("col1") should equal (33)
    }

    it("Подмена toOne") {
      EntityConfiguration.store = new PS{
        override def select(s: Select, parameters: Map[String, Any]) = Seq(collection.mutable.Map[String, Any]("id" -> 333,
          "article" -> collection.mutable.Map[String, Any]("id" -> 1, "col1" -> 222, "col2" -> 0)))
      }
      val em = nem
      val id = new OneEntityId(dataSource, invoice, 1)
      val e = em.get(id).get
      val a = e("article").asInstanceOf[Entity]
      a.id should equal (new OneEntityId(dataSource, article, 1))
      a("col1") should equal (222)
    }

    store2 = new PS{
      def m1 = collection.mutable.Map[String, Any]("id" -> 1, "col1" -> 22, "col2" -> 0)
      def m2 = collection.mutable.Map[String, Any]("id" -> 2, "col1" -> 33, "col2" -> 0)
      def m3 = collection.mutable.Map[String, Any]("id" -> 3, "col1" -> 44, "col2" -> 0)

      override def select (select: Select, parameters: Map[String, Any]) = select.where match {
        case None => Seq(m1, m2, m3)
        case Some(Equal(Ref("id"), Const(1))) => Seq(m1)
        case _ => Seq()
      }
    }
    it("Считывание из неосновного источника данных") {
      val em = nem
      val entities = em.select(Select(FromEntity(article, None, DataSourceExpressionDataSource(ds2))))
      entities.size should equal (3)
      val e1 = entities.head
      e1.id should equal(new OneEntityId(ds2, article, 1))
      e1("col1") should equal (22)
      val e2 = entities.tail.head
      e2.id should equal(new OneEntityId(ds2, article, 2))
      e2("col1") should equal (33)
    }
    it("Сущности должна получасться по идентификатору из неосновного источника данных") {
      val id = new OneEntityId(ds2, article, 1)
      val e = nem.get(id).get
      e.id should equal(id)
      e("col1") should equal (22)
    }
  }

  describe("Добавление сущностей") {
    it("Сущность должна добавляться") {
      var ok = false
      EntityConfiguration.store = new PS{
        override def insert(statement: Insert, parameters: Map[String, Any]) = {
          statement should equal(
            Insert(FromEntity(article, None, DataSourceExpressionDataSource(dataSource)),
              Seq(InsertColumn("col2", Const(20)), InsertColumn("col1", Const(555)))))
          ok = true
          111
        }
      }
      val em = nem
      em.beginTransaction()
      val e = em.insert(article)
      e("id") should equal(null)
      e("col1") should equal(null)
      e("col2") should equal(20)
      e.update("col1", 555)
      em.commit()

      e.id should equal(new OneEntityId(dataSource, article, 111))
      e("id") should equal(111)
      e("col1") should equal (555)
      e("col2") should equal (20)
      ok should equal (true)
    }
    it("Сущность должна добавляться куда следует") {
      var ok = false
      store2 = new PS{
        override def insert(statement: Insert, parameters: Map[String, Any]) = {
          statement should equal(
            Insert(FromEntity(article, None, DataSourceExpressionDataSource(ds2)),
              Seq(InsertColumn("col2", Const(20)))))
          ok = true
          112
        }
      }
      val em = nem
      em.beginTransaction()
      val e = em.insert(article, ds2)
      em.commit()

      e.id should equal(new OneEntityId(ds2, article, 112))
      e("id") should equal(112)
      ok should equal (true)
    }
  }

  describe("Изменение сущностей") {
    it("Сущность должна изменяться") {
      var ok = false
      EntityConfiguration.store = new PS {
        override def select(s: Select, parameters: Map[String, Any]) =
          Seq(collection.mutable.Map[String, Any]("id" -> 222, "col1" -> 11, "col2" -> 0))

        override def update(update: Update, parameters: Map[String, Any]) {
          update should equal(
            Update(FromEntity(article, Some("t"), DataSourceExpressionDataSource(dataSource)), Seq(UpdateColumn("col1", Const(5565))),
              Some(Equal(Dot("t", "id"), Const(222)))))
          ok = true
        }
      }
      val em = nem
      em.beginTransaction()
      val e = em.get(new OneEntityId(dataSource, article, 222)).get
      e.update("col1", 5565)
      em.commit()

      ok should equal (true)
    }

    it("Сущность должна изменяться там, где нужно") {
      var ok = false
      store2 = new PS{
        override def select(s: Select, parameters: Map[String, Any]) =
          Seq(collection.mutable.Map[String, Any]("id" -> 1, "col1" -> 1, "col2" -> 0))

        override def update(update: Update, parameters: Map[String, Any]) {
          update should equal(
            Update(FromEntity(article, Some("t"), DataSourceExpressionDataSource(ds2)), Seq(UpdateColumn("col1", Const(678))),
              Some(Equal(Dot("t", "id"), Const(1)))))
          ok = true
        }
      }
      val em = nem
      em.beginTransaction()
      val e = em.get(new OneEntityId(ds2, article, 1)).get
      e.update("col1", 678)
      em.commit()

      ok should equal (true)
    }

    it("ToMany") {
      EntityConfiguration.store = new PS{
        override def select(s: Select, parameters: Map[String, Any]) = s.from.asInstanceOf[FromEntity].entity match {
          case e if e == article => Seq(collection.mutable.Map[String, Any]("id" -> 333, "col1" -> 11, "col2" -> 0))
          case e if e == invoice =>{
            Seq(
              M[String, Any]("id" -> 1, "article" -> M[String, Any]("id" -> 333), "col1" -> 11),
              M[String, Any]("id" -> 2, "article" -> M[String, Any]("id" -> 333), "col1" -> 22))
          }
        }

        override def insert(i: Insert, parameters: Map[String, Any]) = {
          i should equal(
            Insert(From(invoice), Seq(InsertColumn("article", Const(333)), InsertColumn("col1", Const(33)))))
          3
        }

        override def delete(d: Delete, parameters: Map[String, Any]) {
          d should equal(
            Delete(From(invoice, "t"),
              Some(Equal(Dot("t", "id"), Const(2)))))
        }
      }
      val em = nem
      val e = em.get(new OneEntityId(dataSource, article, 333)).get
      val invoices = e("invoices").asInstanceOf[Traversable[Entity]]
      invoices.size should equal(2)
      val c = em.insert(invoice)
      c.update("col1", 33)
      e.update("invoices", Seq(invoices.head, c))

    }
  }

  describe("Удаление сущностей") {
    it("Сущность должна удаляться") {
      var ok = false
      EntityConfiguration.store = new PS{
        override def select(s: Select, parameters: Map[String, Any]) =
          Seq(collection.mutable.Map[String, Any]("id" -> 333, "col1" -> 11, "col2" -> 0))

        override def delete(delete: Delete, parameters: Map[String, Any]) {
          delete should equal(
            Delete(FromEntity(article, Some("t"), DataSourceExpressionDataSource(dataSource)),
              Some(Equal(Dot("t", "id"), Const(333)))))
          ok = true
        }
      }

      val em = nem
      em.beginTransaction()
      val e = em.get(new OneEntityId(dataSource, article, 333)).get
      e.update("col1", 123)
      e.delete()
      em.commit()

      ok should equal (true)
    }

    it("Сущность должна удаляться из нужного dataSource") {
      var ok = false
      store2 = new PS{
        override def select(s: Select, parameters: Map[String, Any]) =
          Seq(collection.mutable.Map[String, Any]("id" -> 1, "col1" -> 1, "col2" -> 0))

        override def delete(delete: Delete, parameters: Map[String, Any]) {
          delete should equal(
            Delete(FromEntity(article, Some("t"), DataSourceExpressionDataSource(ds2)),
              Some(Equal(Dot("t", "id"), Const(1)))))
          ok = true
        }
      }

      val em = nem
      em.beginTransaction()
      val e = em.get(new OneEntityId(ds2, article, 1)).get
      e.update("col1", 123)
      e.delete()
      em.commit()

      ok should equal (true)
    }
  }

  describe("lazyLoad") {
    it("ToMany") {
      EntityConfiguration.store = new PS{
        override def select(s: Select, parameters: Map[String, Any]) = s.from.asInstanceOf[FromEntity].entity match {
          case e if e == article => Seq(collection.mutable.Map[String, Any]("id" -> 333, "col1" -> 11, "col2" -> 0))
          case e if e == invoice =>{
            Seq(
              collection.mutable.Map[String, Any]("id" -> 1),
              collection.mutable.Map[String, Any]("id" -> 2))
          }
        }
      }
      val em = nem
      val e = em.get(new OneEntityId(dataSource, article, 333)).get
      val invoices = e("invoices").asInstanceOf[Traversable[Entity]]
      invoices.size should equal(2)
    }

    it("ToOne") {
      EntityConfiguration.store = new PS{
        override def select(s: Select, parameters: Map[String, Any]) = s.from.asInstanceOf[FromEntity].entity match {
          case e if e == article => Seq(collection.mutable.Map[String, Any]("id" -> 12, "col1" -> 11, "col2" -> 0))
          case e if e == invoice =>{Seq(collection.mutable.Map[String, Any]("id" -> 1, "article" -> 12))}
        }
      }
      val em = nem
      val e = em.get(new OneEntityId(dataSource, invoice, 1)).get
      val a = e("article").asInstanceOf[Entity]
      a("col1") should equal(11)
    }

    it("ToOne если ничего не возвращает, то null") {
      EntityConfiguration.store = new PS{
        override def select(s: Select, parameters: Map[String, Any]) = s.from.asInstanceOf[FromEntity].entity match {
          case e if e == article => Seq()
          case e if e == invoice =>{Seq(collection.mutable.Map[String, Any]("id" -> 1, "article" -> 12))}
        }
      }
      val em = nem
      val e = em.get(new OneEntityId(dataSource, invoice, 1)).get
      e("article") should equal(null)
    }
  }
}