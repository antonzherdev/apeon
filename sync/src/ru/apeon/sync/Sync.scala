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

  def sync(env : Environment, source : Entity, destinationDescription : Description,
           dataSource : Option[Expression], where : Option[eql.Expression] = None,
           func : Option[BuiltInFunction] = None) : Entity =
  {
    val aliases = func.map{ f =>
      val pars = f.parameters
      (pars(0).name, pars(1).name)
    }.getOrElse(("s", "d"))

    var w = where.get.map{
      case ref : eql.Dot => replaceRef(source, aliases._1, ref) match {
        case Some(a) => eql.Const(a)
        case None => ref
      }
    }

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