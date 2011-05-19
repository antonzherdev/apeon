package ru.apeon.core.script

import ru.apeon.core.entity._

abstract class ScriptDataTypeEntity extends ScriptDataType {
  def description : Description
  override lazy val declarations =
    description.declarations ++ ScriptDataTypeDescription.declarations(classOf[ScriptDataTypeEntity])
  override def equals(obj: Any) = obj match {
    case e : ScriptDataTypeEntity => e.description == this.description
    case _ => false
  }
}

object ScriptDataTypeEntity {
  def unapply(e : ScriptDataTypeEntity) : Option[Description] = Some(e.description)
}

case class ScriptDataTypeEntityByName(entityName : String) extends ScriptDataTypeEntity {
  var description : Description = _


  override def preFillRef(env : Environment, imports: Imports) {
    description = env.entityDescription(entityName, Some(imports))
  }
}

case class ScriptDataTypeObject(query : ObjectBase) extends ScriptDataType {
  override lazy val declarations = query.declarations
}
case class ScriptDataTypeEntityByDescription(description : Description) extends ScriptDataTypeEntity

object ScriptDataTypeEntityTypeDescription {
  def declarations = Seq(copy, delete)

  def delete = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) {
      env.ref.asInstanceOf[Entity].delete()
    }
    def name = "delete"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) = ScriptDataTypeUnit()
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty || parameters.get.isEmpty
  }

  def copy = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      val source = env.ref.asInstanceOf[Entity]
      source.copy(
        parameters match {
          case None => source.id.description
          case Some(Seq()) => source.id.description
          case Some(Seq(p)) => p.value.asInstanceOf[Description]
        },
        env.dataSource(dataSource).getOrElse(source.id.dataSource))
    }
    def name = "copy"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) = parameters match {
      case None =>  env.dotType.get
      case Some(Seq()) => env.dotType.get
      case Some(Seq(p)) =>  ScriptDataTypeEntityByDescription(
        p.expression.dataType(env).asInstanceOf[ScriptDataTypeEntityDescription].description)
    }

    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters match {
      case None => true
      case Some(Seq()) => true
      case Some(Seq(p)) => p.expression.dataType(env).isInstanceOf[ScriptDataTypeEntityDescription]
      case _ => false
    }
  }
}