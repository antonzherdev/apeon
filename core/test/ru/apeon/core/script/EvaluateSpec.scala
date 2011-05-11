package ru.apeon.core.script

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import ru.apeon.core._
import entity._
import collection.Map
import eql.{Update, Insert, Delete, Select}

class EvaluateSpec extends Spec with ShouldMatchers with EntityDefine with ScriptDefine{
  val tests = collection.mutable.Map[String, Int]()

  case class Test(name : String) extends Expression {
    def evaluate(env: Environment) {
      tests.getOrElseUpdate(name, 0)
      tests.update(name, tests(name) + 1)
    }
    def dataType(env: Environment) = ScriptDataTypeUnit()
    def dataType = ScriptDataTypeUnit()
    def fillRef(env: Environment, imports: Imports) {}

    def preFillRef(model: ObjectModel, imports: Imports) {}
  }

  val sh = new DefaultObjectModel
  val pack = Package(sh, "ru.apeon.test", "1.0.0")
  val ds = new DataSource(pack, "ds") {
    override def store = new PersistentStore {
      def name = null
      def update(update: Update, parameters: Map[String, Any]) = null
      def rollback() {}
      def insert(insert: Insert, parameters: Map[String, Any]) = null
      def delete(delete: Delete, parameters: Map[String, Any]) {}
      def commit() {}
      def beginTransaction() {}
      override def select(select: Select, parameters: Map[String, Any]) = select.from.asInstanceOf[eql.FromEntity].entity match {
        case  a if a == article => select.columns match {
          case Seq(c) => Seq(collection.mutable.Map(c.name -> 1))
          case Seq(c1, c2) => Seq(collection.mutable.Map(c1.name -> 1, c2.name -> 2))
          case _ => Seq()
        }
        case _ => Seq()
      }
    }
  }
  sh.addDataSource(ds)
  val pack2 = Package(sh, "ru.apeon.test2", "1.0.0")
  val col1 = Attribute(pack, "col1", "col1", AttributeDataTypeInteger())
  val col2 = Attribute(pack, "col2", "col2", AttributeDataTypeInteger())
  val plus = Def("plus", Plus(Ref("col1"), Ref("col2")))
  val toStr = Def("toStr", Dot("%d %d", Ref("format", Some(Seq(Par(Ref("col1")), Par(Ref("col2")))))))
  val article = Description(pack, "Article", Table("dba", "article"), Seq(Id, col1, col2, plus, toStr))
  val obj = Object(pack, "Article", Seq(Def("test", ConstInt(11))))
  sh.addEntityDescription(article)
  sh.addObj(obj)
  EntityConfiguration.model = sh
  FillRef(sh, pack, pack, article, obj)
  val M = collection.mutable.Map

  val emptyEm = new EmptyEntityManager
  val article1 = new Entity(emptyEm, new SqlEntityId(EntityConfiguration.dataSource, article, 1), M("id" -> 1, "col1" -> 10, "col2" -> 12))
  val article2 = new Entity(emptyEm, new SqlEntityId(EntityConfiguration.dataSource, article, 2), M("id" -> 2, "col1" -> 132, "col2" -> 122))


  class TestedEntityManager extends EmptyEntityManager {
    override def get(id: EntityId) = id.description match {
      case a if a == article => id match {
        case id : SqlEntityId => id.id match {
          case 1 => Some(article1)
          case _ => None
        }
        case _ => None
      }
      case _ => None
    }

    override def select(select: eql.Select) = select.from.asInstanceOf[eql.FromEntity].entity match {
      case  a if a == article => select.where match {
        case None => Seq(article1, article2)
        case Some(eql.Equal(eql.Ref(Some("a"), "id"), eql.Const(1))) => {
          Seq(article1)
        }
        case Some(eql.Equal(eql.Ref(None, "col1"), eql.Const(10))) => {
          Seq(article1)
        }
        case Some(eql.Equal(eql.Ref(None, "col1"), e : EqlExternalScript)) => {
          e.data should equal(10)
          Seq(article1)
        }
        case _ => Seq()
      }
      case _ => Seq()
    }
  }


  def run(statement : Statement*) = Script(pack, statement : _*).evaluate(new Env(new TestedEntityManager))
  def run(pack : Package, statement : Statement*) = Script(pack, statement : _*).evaluate(new Env(new TestedEntityManager))
  def run(env : Environment, statement : Statement*) = Script(pack, statement : _*).evaluate(env)
  def run(em : EntityManager, statement : Statement*) = Script(pack, statement : _*).evaluate(env = new Env(em))

  describe("Script") {
    it("eval") {
      tests.clear()
      run(
        Test("1"),
        Test("2"))
      tests should equal (Map("1" -> 1, "2" -> 1))
    }
  }


  describe("Parentheses") {
    it("Скобочки") {
      run(
        Parentheses(Seq(
          Val("a", ConstInt(3333)),
          Ref("a")
        ))
      ) should equal (3333)
    }

    it("Объявление удаляется за скобками") {
      evaluating{
        run(
          Parentheses(Seq(
            Val("a", ConstInt(3333))
          )),
          Ref("a")
        )
      } should produce[ScriptException]
    }
  }

  describe("Seq") {
    it("foreach") {
      run(
        Var("sum", ConstInt(0)),
        Dot(ConstSeq(Seq(ConstInt(111), ConstInt(222))), ref("foreach",
          BuiltInFunction(Set(Ref("sum"), Plus(Ref("sum"), Ref("r"))), Seq("r")))
        ),
        Ref("sum")
      ) should equal (333)
    }

    it("filter") {
      val s = ConstSeq(Seq(5, 1, 4, 2))
      val f = BuiltInFunction(More(Ref("_"), 3))
      run(Dot(s, ref("filter", f))) should equal (Seq(5, 4))
      run(Dot(s, ref("filterNot", f))) should equal (Seq(1, 2))
    }
  }

  describe("Eql") {
    it("Запрос сущностей") {
      run(
        Dot(ConstEql("from Article"), Ref("select"))
      ) should equal (Seq(article1, article2))
    }

    it("Запрос сущностей с условием") {
      run(
        Dot(ConstEql("from Article as a where a.id = 1"), Ref("select"))
      ) should equal (Seq(article1))
    }

    it("Запрос одной сущности с условием") {
      run(
        Dot(ConstEql("from Article as a where a.id = 1"), Ref("get"))
      ) should equal (article1)
    }

    it("Запрос со встроенным скриптом") {
      run(
        Val("col1", ConstInt(10)),
        Dot(ConstEql("from Article where col1 = %col1%"), Ref("select"))
      ) should equal (Seq(article1))
    }

    it("Запрос одной колонки") {
      run(
        Dot(ConstEql("select 1 from Article where id = 1"), Ref("get"))
      ) should equal (1)
    }

    it("Запрос нескольких колонки") {
      run(
        Dot(Dot(ConstEql("select 1 as one, 2 as two from Article where id = 1"), Ref("get")), Ref("two"))
      ) should equal (2)
    }
    //TODO: Тесты на другие варианты запросов
  }

  describe("if") {
    it("True") {
      run(If(Equal(ConstInt(1), ConstInt(1)), ConstString("True"), Some(ConstString("False")))) should equal("True")
    }
    it("False") {
      run(If(NotEqual(ConstInt(1), ConstInt(1)), ConstString("True"), Some(ConstString("False")))) should equal("False")
    }
  }

  describe("Dot") {
    it("Прямая ссылка на пакет") {
      run(pack2, Dot(Dot(Dot(Ref("ru"), Ref("apeon")), Ref("test")), Ref("Article", ConstInt(1))))should equal(article1)
    }

    it("Ссылка на с импортом") {
      run(pack2, Import("ru.apeon._"), Dot(Ref("test"), Ref("Article", ConstInt(1))))should equal(article1)
    }
  }

  describe("Сущность") {
    it("Колонка") {
      run(
        Dot(Ref("Article", ConstInt(1)), Ref("col1"))
      ) should equal (10)
    }

    it("Insert") {
      var ok = false
      var e : Entity = null
      val em = new EmptyEntityManager {
        override def insert(description: Description, dataSource: DataSource) = {
          description should equal(article)
          ok = true
          e = new Entity(this, new SqlEntityId(dataSource, description, -1))
          e
        }
      }
      run(em,
        Dot(Ref("Article"), Ref("insert"))
      ) should equal (e)
      ok should equal (true)
    }

    it("GetEntity by id") {
      run(
        Ref("Article", ConstInt(1))
      ) should equal (article1)
    }

    it("GetEntity by eql") {
      run(
        Ref("Article", ConstEql("col1 = 10"))
      ) should equal (article1)
    }

    it("GetEntity by eql with") {
      run(
        Val("col1", ConstInt(10)),
        Ref("Article", ConstEql("col1 = %col1%"))
      ) should equal (article1)
    }

    it("Если существует объект, который называется также как сущность, то он должен считаться отнаследованным от объекта сущности") {
      run(
        Dot(Ref("Article"), Ref("test"))
      ) should equal (11)
    }
  }

  describe("Compare") {
    it("==") {
      run(Equal(ConstInt(1), ConstInt(1))) should equal (true)
      run(Equal(ConstInt(1), ConstInt(2))) should equal (false)
    }
    it("!=") {
      run(NotEqual(ConstInt(1), ConstInt(1))) should equal (false)
      run(NotEqual(ConstInt(1), ConstInt(2))) should equal (true)
    }
    it(">") {
      run(More(ConstInt(1), ConstInt(1))) should equal (false)
      run(More(ConstInt(2), ConstInt(1))) should equal (true)
      run(More(ConstInt(1), ConstInt(2))) should equal (false)
    }
    it(">=") {
      run(MoreOrEqual(ConstInt(1), ConstInt(1))) should equal (true)
      run(MoreOrEqual(ConstInt(2), ConstInt(1))) should equal (true)
      run(MoreOrEqual(ConstInt(1), ConstInt(2))) should equal (false)
    }
    it("<") {
      run(Less(ConstInt(1), ConstInt(1))) should equal (false)
      run(Less(ConstInt(2), ConstInt(1))) should equal (false)
      run(Less(ConstInt(1), ConstInt(2))) should equal (true)
    }
    it("<=") {
      run(LessOrEqual(ConstInt(1), ConstInt(1))) should equal (true)
      run(LessOrEqual(ConstInt(2), ConstInt(1))) should equal (false)
      run(LessOrEqual(ConstInt(1), ConstInt(2))) should equal (true)
    }
    it("&&") {
      run(And(Equal(ConstInt(1), ConstInt(1)), Equal(ConstInt(2), ConstInt(2)))) should equal (true)
      run(And(Equal(ConstInt(1), ConstInt(1)), Equal(ConstInt(2), ConstInt(3)))) should equal (false)
      run(And(Equal(ConstInt(1), ConstInt(2)), Equal(ConstInt(2), ConstInt(2)))) should equal (false)
    }
    it("||") {
      run(Or(Equal(ConstInt(1), ConstInt(1)), Equal(ConstInt(2), ConstInt(2)))) should equal (true)
      run(Or(Equal(ConstInt(1), ConstInt(1)), Equal(ConstInt(2), ConstInt(3)))) should equal (true)
      run(Or(Equal(ConstInt(1), ConstInt(2)), Equal(ConstInt(2), ConstInt(3)))) should equal (false)
    }
  }

  describe("Ariphmethic") {
    it("+Int") {
      run(Plus(ConstInt(1), ConstInt(2))) should equal(3)
    }
    it("+String") {
      run(Plus(ConstString("test"), ConstString("t"))) should equal("testt")
      run(Plus(ConstString("test"), ConstInt(2))) should equal("test2")
    }
    it("+Collection") {
      run(Plus(ConstSeq(Seq(ConstInt(3), ConstInt(1))), ConstSeq(Seq(ConstInt(2), ConstInt(3))))) should equal(Seq(3, 1, 2, 3))
      run(Plus(ConstSeq(Seq(ConstInt(3), ConstInt(1))), ConstInt(2))) should equal(Seq(3, 1, 2))
    }
    it("-Int") {
      run(Minus(ConstInt(1), ConstInt(2))) should equal(-1)
    }
    it("*Int") {
      run(Mul(ConstInt(2), ConstInt(3))) should equal(6)
    }
    it("/Int") {
      run(Div(ConstInt(4), ConstInt(2))) should equal(2)
    }
  }

  describe("=") {
    it("=") {
      run(
        Var("v", ConstInt(1)),
        Set(Ref("v"), ConstInt(5)),
        Ref("v")
      ) should equal(5)
    }
    it("+=") {
      run(
        Var("v", ConstInt(1)),
        SetPlus(Ref("v"), ConstInt(5)),
        Ref("v")
      ) should equal(6)
    }
    it("-=") {
      run(
        Var("v", ConstInt(1)),
        SetMinus(Ref("v"), ConstInt(5)),
        Ref("v")
      ) should equal(-4)
    }
    it("*=") {
      run(
        Var("v", ConstInt(2)),
        SetMul(Ref("v"), ConstInt(3)),
        Ref("v")
      ) should equal(6)
    }
    it("/=") {
      run(
        Var("v", ConstInt(4)),
        SetDiv(Ref("v"), ConstInt(2)),
        Ref("v")
      ) should equal(2)
    }
  }

  describe("Ref") {
    def pfo(obj : Object) {
      val ds = new DataSource(pack, "ds")
      obj.pack.model.addDataSource(ds)
      obj.pack.model.addObj(obj)
      EntityConfiguration.model = obj.pack.model
      FillRef(obj.pack.model, obj.pack, obj.pack, obj)
    }
    def pc = Package(new DefaultObjectModel, "ru.apeon.test", "1.0.0")
    it("This object ref") {
      val pack = pc
      pfo(Object(pack, "Test", Seq(Def("test", ConstInt(11)), Def("test2", Ref("test")))))
      run(pack, Dot(Ref("Test"), Ref("test2"))) should equal (11)
      EntityConfiguration.model = sh
    }
    it("This object ref in parameters after dot") {
      val pack = pc
      pfo(Object(pack, "Test", Seq(Def("test", ConstInt(11)),
        Def("test2", Dot("%d", Ref("format", Ref("test")))))))
      run(pack, Dot(Ref("Test"), Ref("test2"))) should equal ("11")
      EntityConfiguration.model = sh
    }
    it("Entity field function ref") {
      run(
        Dot(Ref("Article", ConstInt(1)), Ref("plus"))
      ) should equal (22)
    }

    it("Entity field function ref in parameters after dot") {
      run(
        Dot(Ref("Article", ConstInt(1)), Ref("toStr"))
      ) should equal ("10 12")
    }

    //TODO: Должно работать #43
    /*it("This entity ref") {
      val sh = new DefaultEntityModel
      val pack = Package(sh, "ru.apeon.test", "1.0.0")
      val test = Description(pack, "Test", Table("", "test"),
        Seq(Var("v", ConstInt(10)),
        Def("updV", Set(Ref("v"), Plus(Ref("v"), ConstInt(10))))
        ))
      sh.addEntityDescription(test)
      EntityConfiguration.model = sh
      FillRef(sh, pack, pack, test)

      run(pack,
        Val("t", Dot("Test", "insert")),
        Dot("t", "updV"),
        Dot("t", "updV"),
        Dot("t", "v")
      ) should equal (30)
    }*/
  }
}

class EmptyEntityManager extends EntityManager {
  def beginTransaction() {}
  def select(select: eql.Select) : Seq[Entity] = Seq()
  def register(entity: Entity) = null
  def lazyLoad(entity: Entity, field : Field, data : Any) : Any = null
  def insert(description: Description, dataStore: DataSource) : Entity = null
  def get(id: EntityId) : Option[Entity] = None
  def commit() {}
  def beforeUpdate(entity: Entity, key: String, data: Any) {}
  def beforeDelete(entity: Entity) {}
  def afterUpdate(entity: Entity, key: String, data: Any) {}
  def afterInsert(entity: Entity) {}
  def afterDelete(entity: Entity) {}
}
class Env(override val em : EntityManager) extends DefaultEnvironment {
  override protected def createEntityManager = null
}