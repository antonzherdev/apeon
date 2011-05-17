package ru.apeon.core.script

import ru.apeon.core._
import entity._
import eql.DataSourceExpressionDataSource


case class ScriptDataTypeEntityDescription(description : Description) extends ScriptDataType {
  private val obj : Option[ObjectBase] = description.pack.model.objOption(description.fullName)
  override val declarations = Seq(applyId, applyEql, insert, findEql) ++ obj.map(_.declarations).getOrElse(Seq())


  def insert = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.em.insert(description, env.dataSource(dataSource))
    def name = "insert"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) = ScriptDataTypeEntityByDescription(description)
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
  }

  def applyId = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      parameters.get.head.value match {
        case id : Int => env.em.get(new SqlEntityId(env.dataSource(dataSource), description, id)).getOrElse(
          throw ScriptException(env, "Entity not found")
        )
        case _ => throw ScriptException(env, "Not integer")
      }
    def name = "apply"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) = ScriptDataTypeEntityByDescription(description)
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters match {
      case Some(Seq(par)) => par.expression.dataType(env) == ScriptDataTypeInteger()
      case _ => false
    }
  }

  def applyEql = new FindEql {
    override def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression])  =
      super.value(env, parameters, dataSource).asInstanceOf[Option[Entity]].getOrElse{
        throw ScriptException(env, "Entity not found by %s".format(parameters.get.head.value))
      }
    override def name = "apply"
    override def dataType(env: Environment, parameters : Option[Seq[Par]]) = ScriptDataTypeEntityByDescription(description)
  }

  def findEql = new FindEql

  class FindEql extends Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression])  = {
      val where = parameters.get.head.value.asInstanceOf[eql.Expression]
      val select = eql.Select(eql.FromEntity(description, None,
        DataSourceExpressionDataSource(env.dataSource(dataSource))), where = Some(where))
      env.em.select(select) match {
        case Seq(e) => Some(e)
        case Seq() => None
        case _ => throw ScriptException(env, "Found entity more then one by %s".format(select))
      }
    }
    def name = "find"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) : ScriptDataType  = ScriptDataTypeOption(ScriptDataTypeEntityByDescription(description))
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters match {
      case Some(Seq(par)) => par.expression.dataType(env) == ScriptDataTypeEqlExpression()
      case _ => false
    }
  }
}
