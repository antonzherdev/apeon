package ru.apeon.sync

import ru.apeon.core.eql
import ru.apeon.core.entity.{ToMany, Entity, Description}
import ru.apeon.core.script._

object SyncDeclaration extends Declaration {
  def name = "sync"

  def parent(env: Environment) : Option[ParentSync] = env.leftEntity match{
    case Some(par) => par.id.description.field(env.currentSet.get.left.asInstanceOf[Dot].right.name) match {
        case many : ToMany => Some(ParentSync(par, many.toOne))
        case _ => None
      }
    case None => None
  }

  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = env.ref match {
    case source : Entity => value(env, parameters, dataSource, source)
    case sources : Traversable[Entity] => sources.map{
      source => value(env, parameters, dataSource, source)
    }
    case null => null
  }

  def option(value : Int) : SyncOptions =
    SyncOptions(
      sync = if((value & SyncObject.InsertOnly) != 0) {
        InsertOnly()
      } else {
        if((value & SyncObject.UpdateOnly) != 0) UpdateOnly() else InsertUpdate()
      },
      auto =
              if((value & SyncObject.NoAutoUpdate) != 0) NoAutoUpdate() else AutoUpdate(
                one = if((value & SyncObject.NoAutoUpdateToOne) != 0) NoAutoToOne() else AutoToOne(),
                many = if((value & SyncObject.NoAutoUpdateToMany) != 0) NoAutoToMany() else {
                  if((value & SyncObject.ToManyAppend) != 0) AutoToManyAppend() else AutoToManySet()
                }
              )
    )

  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression], source : Entity) = parameters match {
    case Some(Seq(
      ParVal(destinationDescription : Description, _),
      ParVal(where : eql.Expression, _),
      ParVal(func : BuiltInFunction, _)
    )) => Sync.sync(env, source, destinationDescription, dataSource, Some(where), Some(func), parent(env))
    case Some(Seq(
      ParVal(destinationDescription : Description, _),
      ParVal(where : eql.Expression, _)
    )) => Sync.sync(env, source, destinationDescription, dataSource, Some(where), None, parent(env))
    case Some(Seq(
      ParVal(where : eql.Expression, _),
      ParVal(func : BuiltInFunction, _)
    )) => Sync.sync(env, source, source.id.description, dataSource, Some(where), Some(func), parent(env))
    case Some(Seq(
      ParVal(where : eql.Expression, _)
    )) => Sync.sync(env, source, source.id.description, dataSource, Some(where), None, parent(env))
    case Some(Seq(
      ParVal(func : BuiltInFunction, _)
    )) => Sync.sync(env, source, source.id.description, dataSource, None, Some(func), parent(env))
    case None => Sync.sync(env, source, source.id.description, dataSource, None, None, parent(env))
    case Some(Seq(
      ParVal(destinationDescription : Description, _),
      ParVal(where : eql.Expression, _),
      ParVal(opt : Int, _),
      ParVal(func : BuiltInFunction, _)
    )) => Sync.sync(env, source, destinationDescription, dataSource, Some(where), Some(func), parent(env), option(opt))
    case Some(Seq(
      ParVal(destinationDescription : Description, _),
      ParVal(where : eql.Expression, _),
      ParVal(opt : Int, _)
    )) => Sync.sync(env, source, destinationDescription, dataSource, Some(where), None, parent(env), option(opt))
    case Some(Seq(
      ParVal(where : eql.Expression, _),
      ParVal(opt : Int, _),
      ParVal(func : BuiltInFunction, _)
    )) => Sync.sync(env, source, source.id.description, dataSource, Some(where), Some(func), parent(env), option(opt))
    case Some(Seq(
      ParVal(where : eql.Expression, _),
      ParVal(opt : Int, _)
    )) => Sync.sync(env, source, source.id.description, dataSource, Some(where), None, parent(env), option(opt))
    case Some(Seq(
      ParVal(opt : Int, _),
      ParVal(func : BuiltInFunction, _)
    )) => Sync.sync(env, source, source.id.description, dataSource, None, Some(func), parent(env), option(opt))
    case Some(Seq(
      ParVal(opt : Int, _)
    )) => Sync.sync(env, source, source.id.description, dataSource, None, None, parent(env), option(opt))
    case _ => throw ScriptException("Error in parameters.")
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
    case Some(Seq(Par(entity : Ref, _), Par(where : ConstEql, _), Par(opt, _), Par(bf : BuiltInFunction, _))) =>
      entity.dataType(env).isInstanceOf[ScriptDataTypeEntityDescription] && opt.dataType(env) == ScriptDataTypeInteger()
    case Some(Seq(Par(entity : Ref, _), Par(where : ConstEql, _), Par(opt, _))) =>
      entity.dataType(env).isInstanceOf[ScriptDataTypeEntityDescription] && opt.dataType(env) == ScriptDataTypeInteger()
    case Some(Seq(Par(where : ConstEql, _), Par(opt, _), Par(bf : BuiltInFunction, _))) => opt.dataType(env) == ScriptDataTypeInteger()
    case Some(Seq(Par(where : ConstEql, _), Par(opt, _))) => opt.dataType(env) == ScriptDataTypeInteger()
    case Some(Seq(Par(opt, _), Par(bf : BuiltInFunction, _))) => opt.dataType(env) == ScriptDataTypeInteger()
    case Some(Seq(Par(opt, _)))=> opt.dataType(env) == ScriptDataTypeInteger()
    case _ => false
  }

  def tp(env : Environment) : ScriptDataType = env.dotType.get match {
      case ScriptDataTypeSeq(tp) => tp
      case tp => tp
    }

  override def builtInParameters(env: Environment, parameters: Option[Seq[Par]], parameterNumber: Int, parameter: Par) =
    Seq(BuiltInParameterDef("s", tp(env)),
      BuiltInParameterDef("d",
        if(parameters.isDefined && !parameters.get.isEmpty && parameters.get.head.expression.isInstanceOf[Ref])
          ScriptDataTypeEntityByDescription(parameters.get.head.expression.dataType(env).asInstanceOf[ScriptDataTypeEntityDescription].description)
        else tp(env))
    )
}

