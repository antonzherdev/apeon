package ru.apeon.core.script

import ru.apeon.core._
import entity._
import eql.DataSourceExpressionDataSource


abstract class ScriptDataTypeBaseEntityDescription extends ScriptDataType {
  override def correspond(dataType: ScriptDataType) = dataType.isInstanceOf[ScriptDataTypeBaseEntityDescription]
}

case class ScriptDataTypeEntityDescriptionTemplate() extends ScriptDataTypeBaseEntityDescription
case class ScriptDataTypeEntityDescription(model : ObjectModel, description : Description) extends ScriptDataTypeBaseEntityDescription {
  private lazy val obj : Option[ObjectBase] = model.objOption(description.fullName)
  override lazy val declarations =
    ScriptDataTypeDescription.declarations(classOf[ScriptDataTypeEntityDescription]) ++
            obj.map(_.declarations).getOrElse(Seq()) ++
            description.fields.filter(_.isInstanceOf[ToManyBuiltIn]).map{
              field => new Declaration {
                def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
                  field.asInstanceOf[ToManyBuiltIn].entity
                def dataType(env: Environment, parameters: Option[Seq[Par]]) =
                  ScriptDataTypeEntityDescription(model, field.asInstanceOf[ToManyBuiltIn].entity)
                def name = field.name
              }
            }
}

object ScriptDataTypeEntityDescriptionTypeDescription {
  def declarations = Seq(applyId, applyEql, findEql, insert, findId, firstEql, firstOptionEql)

  def tp(env : Environment) = env.dotType.get.asInstanceOf[ScriptDataTypeEntityDescription]
  def des(env : Environment) = tp(env).description

  val insert = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.em.insert(des(env), env.dataSource(dataSource).getOrElse(des(env).dataSource))
    def name = "insert"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) = ScriptDataTypeEntityByDescription(des(env))
  }

  val applyId = new FindId {
    override def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      super.value(env, parameters, dataSource).asInstanceOf[Option[Entity]].getOrElse{
        throw ScriptException("Entity %s not found for id %d".format(des(env), parameters.get.head.value))
      }
    override def name = "apply"
    override def dataType(env: Environment, parameters : Option[Seq[Par]]) = ScriptDataTypeEntityByDescription(des(env))
  }

  def findId = new FindId

  class FindId extends Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) : Any =
      parameters.get.head.value match {
        case id : Int => env.em.get(new OneEntityId(env.dataSource(dataSource).getOrElse(des(env).dataSource), des(env), id))
        case _ => throw ScriptException("Not Int")
      }
    def name = "find"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) : ScriptDataType =
      ScriptDataTypeOption(ScriptDataTypeEntityByDescription(des(env)))
    override def parameters = Seq(DefPar("id", ScriptDataTypeInteger()))
  }

  val applyEql = new EqlBase {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression])  =
      select(env, parameters, dataSource)  match {
        case Seq(e) => e
        case Seq() => throw ScriptException("Entity not found by %s".format(parameters.get.head.value))
        case _ => throw ScriptException("Found entity more then one")
      }
    def name = "apply"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) = ScriptDataTypeEntityByDescription(des(env))
  }
  val findEql = new EqlBase {
    def name = "find"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) = ScriptDataTypeOption(ScriptDataTypeEntityByDescription(des(env)))
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) : Any  = {
      select(env, parameters, dataSource)  match {
        case Seq(e) => Some(e)
        case Seq() => None
        case _ => throw ScriptException("Found entity more then one")
      }
    }
  }
  val firstEql = new EqlBase {
    def name = "first"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) = ScriptDataTypeEntityByDescription(des(env))
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      select(env, parameters, dataSource).head
  }
  val firstOptionEql = new EqlBase {
    def name = "firstOption"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) = ScriptDataTypeOption(ScriptDataTypeEntityByDescription(des(env)))
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      select(env, parameters, dataSource).headOption
  }

  abstract class EqlBase extends Declaration {
    def select(env: Environment, parameters: Option[scala.Seq[ParVal]], dataSource: Option[Expression]): Seq[Entity] = {
      val where = parameters.get.head.value.asInstanceOf[eql.Expression]
      val select = eql.Select(eql.FromEntity(des(env), None,
        DataSourceExpressionDataSource(env.dataSource(dataSource).getOrElse {
          des(env).dataSource
        })), where = Some(where))
      env.em.select(select)
    }
    override def parameters = Seq(DefPar("where", ScriptDataTypeEqlExpression()))
  }
}