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
  def declarations = Seq(copy0, copy1, delete, fAsInstanceOf, fAsInstanceOfOption, fIsInstanceOf, HashCodeDeclaration)

  def delete = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) {
      env.ref.asInstanceOf[Entity].delete()
    }
    def name = "delete"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) = ScriptDataTypeUnit()
  }

  def copy0 = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      val source = env.ref.asInstanceOf[Entity]
      source.copy(source.id.description, env.dataSource(dataSource).getOrElse(source.id.dataSource))
    }
    def name = "copy"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) = env.dotType.get
  }

  def copy1 = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      val source = env.ref.asInstanceOf[Entity]
      source.copy(parameters.get.head.value.asInstanceOf[Description],
        env.dataSource(dataSource).getOrElse(source.id.dataSource))
    }
    def name = "copy"
    def dataType(env: Environment, parameters : Option[Seq[Par]]) = ScriptDataTypeEntityByDescription(
      parameters.get.head.expression.dataType(env).asInstanceOf[ScriptDataTypeEntityDescription].description)
    override def parameters = Seq(DefPar("entity", ScriptDataTypeEntityDescriptionTemplate()))
  }

  val fAsInstanceOfOption = new Declaration {
    def name = "asInstanceOfOption"

    def dataType(env: Environment, parameters: Option[Seq[Par]]) =
      ScriptDataTypeOption(ScriptDataTypeEntityByDescription(
        parameters.get.head.expression.dataType(env).asInstanceOf[ScriptDataTypeEntityDescription].description
      ))

    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      val s = env.ref.asInstanceOf[Entity]
      val d = parameters.get.head.value.asInstanceOf[Description]
      if(s.id.description.isInheritFrom(d)) {
        Some(s)
      } else {
        if(d.isInheritFrom(s.id.description)) {
          env.em.get(s.id.idFor(d))
        }
        else {
          None
        }
      }
    }
    override def parameters = Seq(DefPar("entity", ScriptDataTypeEntityDescriptionTemplate()))
  }

  val fAsInstanceOf = new Declaration {
    def name = "asInstanceOf"

    def dataType(env: Environment, parameters: Option[Seq[Par]]) =
      ScriptDataTypeEntityByDescription(
        parameters.get.head.expression.dataType(env).asInstanceOf[ScriptDataTypeEntityDescription].description
      )

    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      fAsInstanceOfOption.value(env, parameters, dataSource).asInstanceOf[Option[Entity]].getOrElse {
        throw ScriptException("Entity %s can not be instance of %s.".format(env.ref, parameters.get.head))
      }
    }
    override def parameters = Seq(DefPar("entity", ScriptDataTypeEntityDescriptionTemplate()))
  }

  val fIsInstanceOf = new Declaration {
    def name = "isInstanceOf"

    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeBoolean()

    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      fAsInstanceOfOption.value(env, parameters, dataSource).asInstanceOf[Option[Entity]].isDefined
    }
    override def parameters = Seq(DefPar("entity", ScriptDataTypeEntityDescriptionTemplate()))
  }
}