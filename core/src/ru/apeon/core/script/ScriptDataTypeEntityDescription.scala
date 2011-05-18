package ru.apeon.core.script

import ru.apeon.core._
import entity._
import eql.DataSourceExpressionDataSource



case class ScriptDataTypeEntityDescription(model : ObjectModel, description : Description) extends ScriptDataType {
  private val obj : Option[ObjectBase] = model.objOption(description.fullName)
  override val declarations =
    Seq(applyId, applyEql, findEql, insert, findId) ++
          obj.map(_.declarations).getOrElse(Seq()) ++
          description.fields.filter(_.isInstanceOf[ToManyBuiltIn]).map{
            field => new Declaration {
              def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
                field.asInstanceOf[ToManyBuiltIn].entity
              def dataType(env: Environment, parameters: Option[Seq[Par]]) =
                ScriptDataTypeEntityDescription(model, field.asInstanceOf[ToManyBuiltIn].entity)
              def name = field.name
              def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
            }
          }

  def insert = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.em.insert(description, env.dataSource(dataSource).getOrElse(description.dataSource))
    def name = "insert"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) = ScriptDataTypeEntityByDescription(description)
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
  }

  def applyId = new FindId {
    override def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      super.value(env, parameters, dataSource).asInstanceOf[Option[Entity]].getOrElse{
          throw ScriptException(env, "Entity %s not found for id %d".format(description, parameters.get.head.value))
      }
    override def name = "apply"
    override def dataType(env: Environment, parameters : Option[Seq[Par]]) = ScriptDataTypeEntityByDescription(description)
  }

  def findId = new FindId

  class FindId extends Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) : Any =
      parameters.get.head.value match {
        case id : Int => env.em.get(new OneEntityId(env.dataSource(dataSource).getOrElse(description.dataSource), description, id))
        case _ => throw ScriptException(env, "Not integer")
      }
    def name = "find"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) : ScriptDataType =
      ScriptDataTypeOption(ScriptDataTypeEntityByDescription(description))
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
    override  def name = "apply"
    override def dataType(env: Environment, parameters : Option[Seq[Par]]) = ScriptDataTypeEntityByDescription(description)
  }

  def findEql = new FindEql

  class FindEql extends Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) : Any  = {
      val where = parameters.get.head.value.asInstanceOf[eql.Expression]
      val select = eql.Select(eql.FromEntity(description, None,
        DataSourceExpressionDataSource(env.dataSource(dataSource).getOrElse{description.dataSource})), where = Some(where))
      env.em.select(select) match {
        case Seq(e) => Some(e)
        case Seq() => None
        case _ => throw ScriptException(env, "Found entity more then one by %s".format(select))
      }
    }
    def name = "find"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) : ScriptDataType = ScriptDataTypeOption(ScriptDataTypeEntityByDescription(description))
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters match {
      case Some(Seq(par)) => par.expression.dataType(env) == ScriptDataTypeEqlExpression()
      case _ => false
    }
  }
}
