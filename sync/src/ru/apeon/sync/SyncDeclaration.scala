package ru.apeon.sync

import ru.apeon.core.eql
import ru.apeon.core.entity.{Entity, Description}
import ru.apeon.core.script._

object SyncDeclaration extends Declaration {
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
    case Some(Seq(
      ParVal(where : eql.Expression, _),
      ParVal(func : BuiltInFunction, _)
    )) => Sync.sync(env, source, source.id.description, dataSource, Some(where), Some(func))
    case Some(Seq(
      ParVal(where : eql.Expression, _)
    )) => Sync.sync(env, source, source.id.description, dataSource, Some(where), None)
    case Some(Seq(
      ParVal(func : BuiltInFunction, _)
    )) => Sync.sync(env, source, source.id.description, dataSource, None, Some(func))
    case None => Sync.sync(env, source, source.id.description, dataSource, None, None)
    case _ => throw ScriptException(env, "Error in parameters.")
  }

  def dataType(env: Environment, parameters: Option[Seq[Par]]) =
    if(parameters.isDefined && !parameters.get.isEmpty && parameters.get.head.expression.isInstanceOf[Ref]) {
      val e = parameters.get.head.expression.dataType(env).asInstanceOf[ScriptDataTypeEntityDescription].description
      env.dotType.get match {
        case ScriptDataTypeEntity(_) => ScriptDataTypeEntityByDescription(e)
        case ScriptDataTypeSeq(ScriptDataTypeEntity(_)) =>
          ScriptDataTypeSeq(ScriptDataTypeEntityByDescription(e))
      }
    } else {
      env.dotType.get match {
        case ScriptDataTypeEntity(e) => ScriptDataTypeEntityByDescription(e)
        case ScriptDataTypeSeq(ScriptDataTypeEntity(e)) =>
          ScriptDataTypeSeq(ScriptDataTypeEntityByDescription(e))
      }
    }

  override def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters match {
    case Some(Seq(Par(entity : Ref, _), Par(where : ConstEql, _), Par(bf : BuiltInFunction, _))) =>
      entity.dataType(env).isInstanceOf[ScriptDataTypeEntityDescription]
    case Some(Seq(Par(entity : Ref, _), Par(where : ConstEql, _))) =>
      entity.dataType(env).isInstanceOf[ScriptDataTypeEntityDescription]
    case Some(Seq(Par(where : ConstEql, _), Par(bf : BuiltInFunction, _))) => true
    case Some(Seq(Par(where : ConstEql, _))) => true
    case Some(Seq(Par(bf : BuiltInFunction, _))) => true
    case None => true
    case _ => false
  }

  override def builtInParameters(env: Environment, parameters: Option[Seq[Par]], parameterNumber: Int, parameter: Par) =
    Seq(BuiltInParameterDef("s", env.dotType.get),
      BuiltInParameterDef("d",
        if(parameters.isDefined && !parameters.get.isEmpty && parameters.get.head.expression.isInstanceOf[Ref])
          parameters.get.head.expression.dataType(env)
        else env.dotType.get )
    )
}
