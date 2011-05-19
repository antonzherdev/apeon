package ru.apeon.sync

import ru.apeon.core.eql
import ru.apeon.core.entity.{Entity, Description}
import ru.apeon.core.script._

class SyncDeclaration extends Declaration {
  def name = "sync"
  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = env.ref match {
    case source : Entity => value(env, parameters, dataSource, source)
    case sources : Traversable[Entity] => sources.map{
      source => value(env, parameters, dataSource, source)
    }
  }

  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression], source : Entity) = parameters match {
    case Some(Seq(
      ParVal(destinationDescription : Description, _),
      ParVal(where : eql.Expression, _),
      ParVal(func : BuiltInFunction, _)
    )) => Sync.sync(env, source, destinationDescription, dataSource, Some(where), Some(func))
    case Some(Seq(
      ParVal(destinationDescription : Description, _),
      ParVal(where : eql.Expression, _)
    )) => Sync.sync(env, source, destinationDescription, dataSource, Some(where), None)
    case _ => throw ScriptException(env, "Error in parameters.")
  }

  def dataType(env: Environment, parameters: Option[Seq[Par]]) = env.dotType.get match {
    case ScriptDataTypeEntity(e) => ScriptDataTypeEntityByDescription(e)
    case ScriptDataTypeSeq(ScriptDataTypeEntity(e)) =>
      ScriptDataTypeSeq(ScriptDataTypeEntityByDescription(e))
  }

  def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters match {
    case Some(Seq(Par(entity : Ref, _), Par(where : ConstEql, _), Par(bf : BuiltInFunction, _))) =>
      entity.dataType(env).isInstanceOf[ScriptDataTypeEntityDescription]
    case Some(Seq(Par(entity : Ref, _), Par(where : ConstEql, _))) =>
      entity.dataType(env).isInstanceOf[ScriptDataTypeEntityDescription]
    case _ => false
  }

  override def builtInParameters(env: Environment, parameters: Option[Seq[Par]], parameterNumber: Int, parameter: Par)
  = Seq(BuiltInParameterDef("s", env.dotType.get), BuiltInParameterDef("d", parameters.get.head.expression.dataType(env)))
}