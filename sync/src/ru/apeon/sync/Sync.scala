package ru.apeon.sync

import ru.apeon.core.entity._
import ru.apeon.core.eql
import ru.apeon.core.script._

object Sync {
  private def replaceRef(source : Entity, sourceAlias : String, dot : eql.Dot) : Option[Any] = dot.left match {
    case parent : eql.Dot => replaceRef(source, sourceAlias, parent) match {
      case Some(e : Entity) => Some(e(dot.right.name))
      case _ => None
    }
    case r : eql.Ref => if(r.name == sourceAlias) Some(source(dot.right.name)) else None
    case _ => None
  }

  def syncWhereDeclaration(env: Environment, destinationDescription: Description): Declaration = {
    destinationDescription.declarations.find {
      dec => dec.name == "syncWhere" && dec.isInstanceOf[Def] && dec.asInstanceOf[Def].parameters.isEmpty
    }.getOrElse(throw ScriptException(env, "Sync where is not found."))
  }

  def syncWhere(env: Environment, description: Description, dataSource: Option[Expression]): eql.Expression = {
    syncWhereDeclaration(env, description).value(env, None, dataSource) match {
      case e : eql.Expression => e
      case fields : Seq[String] => {
        val ands = fields.map{field => eql.Equal(eql.Dot("d." + field),  eql.Dot("s." + field))}
        var ret : eql.Expression = ands.head
        for(and <- ands.tail) {
          ret = eql.And(ret, and)
        }
        ret
      }
    }
  }

  //TODO : Where по массиву колонок
  def buildWhere(env: Environment, source: Entity, destinationDescription: Description, where: Option[eql.Expression],
                 aliases: (String, String), dataSource: Option[Expression]): eql.Expression =
  {
    var w : eql.Expression = where.getOrElse {
      if (source.id.description == destinationDescription) {
        syncWhere(env, destinationDescription, dataSource)
      }
      else {
        throw ScriptException(env, "Source entity is not destination but where is not defined.")
      }
    }

    abstract class F { val field : Field}
    case class DestinationField(field : Field) extends F
    case class SourceField(field : Field) extends F
    def field(e: eql.Dot): F = {
      e.left match {
        case eql.Ref(name) if name == aliases._1 => SourceField(source.id.description.field(e.right.name))
        case eql.Ref(name) if name == aliases._2 => DestinationField(destinationDescription.field(e.right.name))
        case d : eql.Dot => field(d) match {
          case DestinationField(o : ToOne) => DestinationField(o.entity.field(e.right.name))
          case SourceField(o : ToOne) => SourceField(o.entity.field(e.right.name))
          case _ => throw ScriptException(env, "Not to one.")
        }
      }
    }
    w = w.map {
      case e @ eql.Equal(left : eql.Dot, right : eql.Dot) => {
        val fields = (field(left), field(right)) match {
          case f @ (SourceField(l), DestinationField(r)) => (l, r, left, right)
          case (DestinationField(l), SourceField(r)) => (r, l, right, left)
        }
        fields match {
          case (l : ToOne, r : ToOne, s, d) => {
            if(l != r) throw ScriptException(env, "Right to one not equals left to one. Use primary key to compare.")
            syncWhere(env, l.entity, dataSource).map{
              case eql.Dot(eql.Ref(name), v) if name == aliases._1 => eql.Dot(s, v)
              case eql.Dot(eql.Ref(name), v) if name == aliases._2 => eql.Dot(d, v)
            }
          }
          case _ => e
        }
      }
    }

    w = w.map {
      case ref: eql.Dot => replaceRef(source, aliases._1, ref) match {
        case Some(a) => eql.Const(a)
        case None => ref
      }
    }
    w
  }

  //TODO: Возможность отключения автоматического прохода вообще, по ссылка toOne и по ссылкам toMany по отдельности
  //TODO: Возможность синхронизации без update. Если такой записи нет, то insert, иначе ничего. Это необходимо для оптимизации.
  def sync(env : Environment, source : Entity, destinationDescription : Description,
           dataSource : Option[Expression], where : Option[eql.Expression] = None,
           func : Option[BuiltInFunction] = None) : Entity =
  {
    val aliases = func.map{ f =>
      val pars = f.parameters
      (pars(0).name, pars(1).name)
    }.getOrElse(("s", "d"))

    var w: eql.Expression = buildWhere(env, source, destinationDescription, where, aliases, dataSource)

    var ds = env.dataSource(dataSource).getOrElse(destinationDescription.dataSource)
    var d : Entity = null
    var toOne : ToOne = null
    var parent : Entity = null
    if(env.leftEntity.isDefined) {
      parent = env.leftEntity.get
      ds = parent.id.dataSource
      parent.id.description.field(env.currentSet.get.left.asInstanceOf[Dot].right.name) match {
        case many : ToMany => {
          if(parent.id.isTemporary) {
            env.em.transaction{}
            d = env.em.insert(destinationDescription, ds)
            d.update(many.toOne, parent)
          }
          else {
            toOne = many.toOne
            w = eql.And(w,
              eql.Equal(eql.Dot(eql.Ref(aliases._2), eql.Ref(many.toOne.name)), parent.id.const))
          }
        }
        case _ => {}
      }
    }

    if(d == null) {
      val select = eql.Select(
        eql.FromEntity(destinationDescription, Some(aliases._2), eql.DataSourceExpressionDataSource(ds)),
        where = Some(w))
      d = env.em.select(select) match {
        case Seq() => env.em.insert(destinationDescription, ds)
        case Seq(e) => e
        case many @ _ =>
          throw ScriptException(env,
"""Many entities to sync.
Source datasource = %s
Source = %s

Destination datasource = %s
Destination = %s""".format(source.id.dataSource.fullName, source, ds.fullName, many))
      }
    }
    if(toOne != null) {
      d.update(toOne, parent)
    }

    //if(auto) {
    //TODO: Автоматический проход по ссылкам ToOne и ToMany
      destinationDescription.attributes.foreach{column =>
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
    //}
    if(func.isDefined) {
      func.get.run(env, source , d)
    }

    d
  }
}