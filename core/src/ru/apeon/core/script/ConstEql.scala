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
        case _ => ScriptDataTypeEqlSelect(select.from.fields.map(f => (f.name, f.scriptDataType)).toMap)
      }
      case Seq(col) => ScriptDataTypeEqlSelectOne(col.expression.dataType(env))
      case _ => ScriptDataTypeEqlSelect(select.columns.map(c => (c.name, c.expression.dataType(env))).toMap)
    }
    case s: eql.Statement => ScriptDataTypeEqlStatement()
    case e : eql.Expression => ScriptDataTypeEqlExpression()
  }

  def value = if(_eql == null) string else _eql
  def expression : eql.Expression = _eql.asInstanceOf[eql.Expression]
  def statement : eql.Statement = _eql.asInstanceOf[eql.Statement]

  override def evaluate(env: Environment) = {
    externals.foreach(_.evaluate(env))
    _eql
  }

  override def fillRef(env: Environment, imports: Imports) {
    val externalCreator = Some({s : String =>
      val external = new EqlExternalScript(ScriptParser.parseStatement(env.model, s))
      external.fillRef(env, imports)
      externals.append(external)
      external
    })
    val parser = new eql.EqlParser(env.model, Some(imports), externalCreator)
    try {
      val stm = parser.parseOption(string)
      if(stm.isDefined) {
        _eql = stm.get
      }else {
        externals.clear()
        _eql = parser.parseExpression(string)
      }
    }
    catch {
      case e : Throwable => throw ScriptException("%s while EQL parsing".format(e.getMessage), Some(e))
    }
  }

  override def toString = "`%s`".format(string)
}

case class ScriptDataTypeEqlExpression() extends ScriptDataType

abstract class ScriptDataTypeEqlSelectBase extends ScriptDataType {
  override lazy val declarations = ScriptDataTypeDescription.declarations(classOf[ScriptDataTypeEqlSelectBase])
  def sel(env : Environment) : eql.Select = env.ref.asInstanceOf[eql.Select]
  def rowDataType : ScriptDataType
  def evaluate(env : Environment) : Seq[Any]
}

object ScriptDataTypeEqlSelectBaseDescription {
  def declarations = Seq(fSelect, fGet, fFind, fFirst, fFirstOption)

  val fSelect : FSelect = new FSelect
  val fGet : FGet = new FGet
  val fFind : FFind = new FFind
  val fFirst = new FFirst
  val fFirstOption = new FFirstOption

  def tp(env : Environment) = env.dotType.get.asInstanceOf[ScriptDataTypeEqlSelectBase]
  def rowDataType(env : Environment) = tp(env).rowDataType
  def evaluate(env : Environment) = tp(env).evaluate(env)

  class FSelect extends Declaration{
    def name = "select"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeSeq(rowDataType(env))
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      evaluate(env)
  }

  class FGet extends Declaration{
    def name = "get"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = rowDataType(env)
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = evaluate(env) match {
      case Seq(ret) => ret
      case Seq() => throw ScriptException("Not found")
      case _ => throw ScriptException( "Have found many but get one")
    }
  }

  class FFind extends Declaration{
    def name = "find"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeOption(rowDataType(env))
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = evaluate(env) match {
      case Seq(ret) => Some(ret)
      case Seq() => None
      case _ => throw ScriptException("Have found many but get one")
    }
  }

  class FFirst extends Declaration{
    def name = "first"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = rowDataType(env)
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = evaluate(env) match {
      case Seq() => throw ScriptException("Not found.")
      case s : Seq[_] => s.head
    }
  }

  class FFirstOption extends Declaration{
    def name = "firstOption"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeOption(rowDataType(env))
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = evaluate(env) match {
      case Seq() => None
      case s : Seq[_] => Some(s.head)
    }
  }
}

case class ScriptDataTypeEqlSelectEntity(description : Description) extends ScriptDataTypeEqlSelectBase {
  val rowDataType = ScriptDataTypeEntityByDescription(description)
  def evaluate(env: Environment) = env.em.select(sel(env))
}
case class ScriptDataTypeEqlSelect(keys : Map[String, ScriptDataType]) extends ScriptDataTypeEqlSelectBase {
  val rowDataType = ScriptDataTypeKeyValue(keys)
  def evaluate(env: Environment) = sel(env).evaluate
}
case class ScriptDataTypeEqlSelectOne(dataType : ScriptDataType) extends ScriptDataTypeEqlSelectBase {
  def rowDataType = dataType
  def evaluate(env: Environment) = sel(env).evaluate.map{_.head._2}
}

case class ScriptDataTypeEqlStatement() extends ScriptDataType

object ScriptDataTypeEqlStatementDescription {
  def declarations = Seq(execute)

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
  }
}