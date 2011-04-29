package ru.apeon.core.script

import ru.apeon.core._
import entity._

abstract class Sync extends Statement
{
  def sourceAlias : String
  def destination : SyncRef
  def where : String
  def statements : Seq[Statement]
  def auto : Boolean

  def apply(env: Environment, entities : Traversable[Entity]) : Traversable[Entity] =
    entities.map{apply(env, _)}

  private def replaceRef(source : Entity, ref : eql.Ref) : Option[Any] = ref.fromRef match {
    case parent : eql.Ref => replaceRef(source, parent) match {
      case Some(e : Entity) => Some(e(ref.name))
      case _ => None
    }
    case _ => ref.from match {
      case Some(a) if a == sourceAlias => Some(source(ref.column))
      case _ => None
    }
  }

  def apply(env: Environment, source : Entity) : Entity = {
    val desEnt = destination.entityDescription
    var w = eql.EqlParser.parseExpression(where, env.model, Some(imports))
    w = w.map{
      case ref : eql.Ref => replaceRef(source, ref) match {
        case Some(a) => eql.Const(a)
        case None => ref
      }
    }

    var dataSource = env.dataSource(destination.dataSource).getOrElse(destination.entityDescription.dataSource)
    var d : Entity = null
    var toOne : ToOne = null
    var parent : Entity = null
    if(env.leftEntity.isDefined) {
      parent = env.leftEntity.get
      dataSource = parent.id.dataSource
      parent.id.description.field(env.currentSet.get.left.asInstanceOf[Dot].right.name) match {
        case many : ToMany => {
          if(parent.id.isTemporary) {
            env.em.transaction{}
            d = env.em.insert(desEnt, dataSource)
            d.update(many.toOne, parent)
          }
          else {
            toOne = many.toOne
            w = eql.And(w,
              eql.Equal(eql.Ref(destination.alias, many.toOne.name), parent.id.const))
          }
        }
        case _ => {}
      }
    }

    if(d == null) {
      val select = eql.Select(eql.FromEntity(desEnt, Some(destination.alias), eql.DataSourceExpressionDataSource(dataSource)), where = Some(w))
      d = env.em.select(select) match {
        case Seq() => env.em.insert(desEnt, dataSource)
        case Seq(e) => e
        case many @ _ =>
          throw ScriptException(env,
"""Many entities to sync.
Source datasource = %s
Source = %s

Destination datasource = %s
Destination = %s""".format(source.id.dataSource.fullName, source, dataSource.fullName, many))
      }
    }
    if(toOne != null) {
      d.update(toOne, parent)
    }

    if(auto) {
      desEnt.attributes.foreach{column =>
        if(!column.isPrimaryKey) {
          source.id.description.fieldOption(column.name) match {
            case Some(a : Attribute) => {
              if(a.dataType == column.dataType) {
                d.update(column, source(a))
              }
            }
            case _ => {}
          }
        }
      }
    }
    env.atomic{
      sourceDeclaration.value = source
      desDeclaration.value = d
      env.addDeclaration(sourceDeclaration)
      env.addDeclaration(desDeclaration)
      statements.foreach{statement =>
        statement.evaluate(env)
      }
    }

    d
  }

  def sourceDataType(env : Environment) : ScriptDataType

  private val sourceDeclaration = new Declaration {
    var value : Entity = _
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = value
    def name = sourceAlias
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = sourceDataType(env)
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
  }
  private val desDeclaration = new Declaration {
    var value : Entity = _
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = value
    def name = destination.alias
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeEntityByDescription(destination.entityDescription)
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
  }

  private var imports : Imports = _
  override def fillRef(env : Environment, imports : Imports) {
    destination.fillRef(env, imports)
    env.atomic {
      env.addDeclaration(sourceDeclaration)
      env.addDeclaration(desDeclaration)
      statements.foreach(stm => env.fillRef(stm, imports))
    }
  }

  def preFillRef(env : Environment, imports: Imports) {
    this.imports = imports
    destination.preFillRef(env, imports)
    statements.foreach(stm => env.preFillRef(stm, imports))
  }
}

/**
 * Результирующая сущность синхронизации
 * @param entity сущность
 * @param alias алиас
 */
case class SyncRef(entityName : String, alias : String, dataSource : Option[Expression] = None) {
  private var _entityDescription : Description = _

  def entityDescription : Description = _entityDescription
  def fillRef(env : Environment, imports : Imports) {
    if(dataSource.isDefined) {
      env.fillRef(dataSource.get, imports)
    }
  }

  def preFillRef(env : Environment, imports : Imports) {
    _entityDescription = env.entityDescription(entityName, Some(imports))
    if(dataSource.isDefined) {
      env.preFillRef(dataSource.get, imports)
    }
  }
}

case class SyncDeclaration(source : SyncRef, destination : SyncRef, where : String, statements : Seq[Statement] = Seq(), auto : Boolean = true)
        extends Sync with Statement
{
  def sourceAlias = source.alias

  def sourceDataType(env: Environment) = ScriptDataTypeEntityByDescription(source.entityDescription)

  def dataType(env : Environment) = ScriptDataTypeSync()

  def evaluate(env: Environment) = this

  override def fillRef(env: Environment, imports: Imports) {
    source.fillRef(env, imports)
    super.fillRef(env, imports)
  }

  override def preFillRef(env : Environment, imports: Imports) {
    source.preFillRef(env, imports)
    super.preFillRef(env, imports)
  }
}


trait SyncExpressionStatement extends Expression {
  def source : Expression

  def dataType(env : Environment) = source.dataType(env) match {
    case e : ScriptDataTypeEntity => e
    case ScriptDataTypeSeq(e: ScriptDataTypeEntity) => ScriptDataTypeSeq(e)
  }

  def sync(env : Environment) : Sync

  def evaluate(env: Environment) = dataType(env) match {
    case ScriptDataTypeEntity(entityDescription) => source.evaluate(env) match {
      case entity : Entity => sync(env).apply(env, entity)
      case o : Object => throw ScriptException(env, "Sync not entity but %s".format(o.getClass.toString))
      case null => null
    }
    case ScriptDataTypeSeq(ScriptDataTypeEntity(entityDescription)) => source.evaluate(env) match {
      case entities : Traversable[Entity] => sync(env).apply(env, entities)
    }
  }
}

/**
 * Синхронизировать конкретную сущность или набор сущностей или отношение один ко многим по настройке,
 * которая определена напрямую без def.
 * @param entity выражение, которое возвращает сущность
 * @param sync настройка
 */
case class SyncEntity(source : Expression, sourceAlias : String,
                      destination : SyncRef, where : String, statements : Seq[Statement] = Seq(), auto : Boolean = true)
        extends Sync with SyncExpressionStatement
{

  def sourceDataType(env: Environment) = source.dataType(env)

  def sync(env: Environment) = this

  override def fillRef(env: Environment, imports: Imports) {
    env.fillRef(source, imports)
    super.fillRef(env, imports)
  }

  override def preFillRef(env : Environment, imports: Imports) {
    env.preFillRef(source, imports)
    super.preFillRef(env, imports)
  }
}

/**
 * Синхронизировать конкретную сущность или набор сущностей или отношение один ко многим по настройке,
 * которая определена с помощью def.
 * @param source выражение, которое возвращает сущность, которую синхронизировать
 * @param by настройка, которая определена в def
 */
case class SyncBy(source : Expression, by : Expression) extends SyncExpressionStatement {
  def sync(env: Environment) =
    by.evaluate(env) match {
      case sync : Sync => sync
    }

  def fillRef(env: Environment, imports: Imports) {
    env.fillRef(by, imports)
    env.fillRef(source, imports)
  }

  def preFillRef(env : Environment, imports: Imports) {
    env.preFillRef(source, imports)
    env.preFillRef(by, imports)
  }
}
