package ru.apeon.core.script

import ru.apeon.core._
import entity._
import collection.mutable.Buffer

case class ConstEql(string : String) extends Constant {
  private var _eql : Any = _
  private val externals = Buffer[EqlExternalScript]()

  def dataType(env: Environment) = _eql match {
     case select : eql.Select => select.columns match {
      case Seq() => select.from match {
        case e : eql.FromEntity => ScriptDataTypeEqlSelectEntity(e.entity)
        case _ => ScriptDataTypeEqlSelect(select)
      }
      case _ => ScriptDataTypeEqlSelect(select)
    }
    case s: eql.Statement => ScriptDataTypeEqlStatement()
    case e : eql.Expression => ScriptDataTypeEqlExpression()
  }

  def value = _eql
  def expression : eql.Expression = _eql.asInstanceOf[eql.Expression]
  def statement : eql.Statement = _eql.asInstanceOf[eql.Statement]

  override def evaluate(env: Environment) = {
    externals.foreach(_.evaluate(env))
    _eql
  }

  override def fillRef(env: Environment, imports: Imports) {
    val externalCreator = Some({s : String =>
      val external = new EqlExternalScript(new ScriptParser(env.model).parseStatement(s))
      external.fillRef(env, imports)
      externals.append(external)
      external
    })
    val parser = new eql.EqlParser(env.model, Some(imports), externalCreator)
    val stm = parser.parseOption(string)
    if(stm.isDefined) {
      _eql = stm.get
    }else {
      externals.clear()
      _eql = parser.parseExpression(string)
    }

  }
}

case class ScriptDataTypeEqlExpression() extends ScriptDataType

case class ScriptDataTypeEqlSelectEntity(description : Description) extends ScriptDataType {
  override def declarations = Seq(select)

  def select = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.em.select(env.ref.asInstanceOf[eql.Select])
    }
    def name = "select"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeSeq(ScriptDataTypeEntityByDescription(description))
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
  }
}

case class ScriptDataTypeEqlSelect(statement : eql.Select) extends ScriptDataType {
  override def declarations = Seq(select)

  def select = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.em.select(statement)
    }
    def name = "select"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeSeq(ScriptDataTypeAny())
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
  }
}

case class ScriptDataTypeEqlStatement() extends ScriptDataType {
  override def declarations = Seq(execute)

  def execute = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.ref match {
        case u : eql.Update => u.dataSource.store.update(u)
        case d : eql.Delete => d.dataSource.store.delete(d)
        case i : eql.Insert => i.dataSource.store.insert(i)
      }
    }
    def name = "execute"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeUnit()
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
  }
}