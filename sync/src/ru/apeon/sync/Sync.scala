package ru.apeon.sync

import ru.apeon.core.entity._
import ru.apeon.core.eql
import ru.apeon.core.script._

object Sync {
  private def replaceRef(source : Entity, sourceAlias : String, dot : eql.Dot) : Option[Any] = dot.left match {
    case parent : eql.Dot => replaceRef(source, sourceAlias, parent) match {
      case Some(e : Entity) => Some(e(dot.right.name))
      case Some(null) => Some(null)
      case _ => None
    }
    case r : eql.Ref => if(r.name == sourceAlias) Some(source(dot.right.name)) else None
    case _ => None
  }

  def syncWhereDeclaration(env: Environment, destinationDescription: Description): Option[Declaration] =
    destinationDescription.declarations.find {
      dec => dec.name == "syncWhere" && dec.isInstanceOf[Def] && dec.asInstanceOf[Def].parameters.isEmpty
    }


  def syncWhere(env: Environment, description: Description, dataSource: Option[Expression]): eql.Expression = {
    syncWhereDeclaration(env, description).getOrElse{
      throw ScriptException("Sync where is not found.")
    }.value(env, None, dataSource) match {
      case e : eql.Expression => e
      case fields : Seq[String] => {
        val ands = fields.map{field => eql.Equal(eql.Dot("d." + field),  eql.Dot("s." + field))}
        var ret : eql.Expression = ands.head
        for(and <- ands.tail) {
          ret = eql.And(ret, and)
        }
        ret
      }
      case e => throw ScriptException("Sync where for entity \"%s\" return unsupported value \"%s\".".format(description.fullName, e))
    }
  }

  def buildWhere(env: Environment, source: Entity, destinationDescription: Description, where: Option[eql.Expression],
                 aliases: (String, String), dataSource: Option[Expression]): eql.Expression =
  {
    var w : eql.Expression = where.getOrElse {
      if (source.id.description == destinationDescription) {
        syncWhere(env, destinationDescription, dataSource)
      }
      else {
        throw ScriptException( "Source entity is not destination but where is not defined.")
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
          case _ => throw ScriptException("Not to one.")
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
            if(l != r) throw ScriptException("Right to one not equals left to one. Use primary key to compare.")
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

  def dataSource(env: Environment, destinationDescription: Description, dataSource: Option[Expression], parent: Option[ParentSync]): DataSource = {
    parent match {
      case Some(par) => par.entity.id.dataSource
      case None => env.dataSource(dataSource).getOrElse(destinationDescription.dataSource)
    }
  }

  def syncFind(env: Environment, source: Entity, destinationDescription: Description,
               dataSourceExpression: Option[Expression],
               where: Option[eql.Expression], parent: Option[ParentSync] = None,
               aliases: (String, String) = ("s", "d") ) : Option[Entity] =
  {
    env.cache.getOrElseUpdate((17, source.id, where).hashCode(), {
      val ds = dataSource(env, destinationDescription, dataSourceExpression, parent)
      var w: eql.Expression = buildWhere(env, source, destinationDescription, where, aliases, dataSourceExpression)
      if (!parent.isDefined || (!parent.get.entity.id.isTemporary && parent.get.checkUnique)) {
        if (parent.isDefined) {
          w = eql.And(w, eql.Equal(eql.Dot(eql.Ref(aliases._2), eql.Ref(parent.get.toOne.name)), parent.get.entity.id.const))
        }
        val select = eql.Select(
          eql.FromEntity(destinationDescription, Some(aliases._2), eql.DataSourceExpressionDataSource(ds)),
          where = Some(w))
        env.em.select(select) match {
          case Seq() => None
          case Seq(e) => Some(e)
          case many@_ =>
            throw ScriptException(
              """Many entities to sync.
            Source datasource = %s
            Source = %s

            Destination datasource = %s
            Destination = %s""".format(source.id.dataSource.fullName, source, ds.fullName, many))
        }
      } else {
        None
      }
    }).asInstanceOf[Option[Entity]]
  }

  def sync(env : Environment, source : Entity, destinationDescription : Description,
           dataSourceExpression : Option[Expression], where : Option[eql.Expression] = None,
           func : Option[BuiltInFunction] = None, parent : Option[ParentSync] = None,
           options : SyncOptions = SyncOptions()) : Entity =
  {
    val aliases = func.map{ f =>
      val pars = f.parameters
      (pars(0).name, pars(1).name)
    }.getOrElse(("s", "d"))

    val found = syncFind(env, source, destinationDescription, dataSourceExpression, where, parent, aliases)
    if(found.isEmpty && UpdateOnly() == options.sync) {
      return null
    }
    val d : Entity = found.getOrElse{
      val ds: DataSource = dataSource(env, destinationDescription, dataSourceExpression, parent)
      val ret = env.em.insert(destinationDescription, ds)
      env.cache.update((17, source.id, where).hashCode(), Some(ret))
      ret
    }

    if(parent.isDefined) {
      d.update(parent.get.toOne, parent.get.entity)
    }

    var funcDestinationAlias = aliases._2
    val syncProc : Option[Def] = if(func.isDefined) None else {
      destinationDescription.declarations.find{
        decl => decl.name == "syncProc" && (decl.parameters match {
          case Seq(DefPar(name, ScriptDataTypeEntity(descr))) => if(descr == destinationDescription) {
            funcDestinationAlias = name
            true
          } else false
          case _ => false
        })
      }.map{_.asInstanceOf[Def]}
    }


    val statements : Seq[Statement] = func.map(_.statement.statements).orElse{syncProc.map{_.statement match {
      case sl : StatementList => sl.statements
      case s => Seq(s)
    }}}.getOrElse{Seq()}

    if(options.sync match {
      case InsertOnly() => found.isEmpty
      case _ => true
    }) {
      val oldDS = env.currentDataSource
      env.setCurrentDataSource(Some(d.id.dataSource))

      options.auto match {
        case AutoUpdate(one, many) => {
          val changedFields : Seq[String] = statements.map{
            case SetBase(Dot(Ref(a, None, None), Ref(f, None, None)), _) if a == funcDestinationAlias =>
              Some(f)
            case Ref("syncSkip", Some(Seq(Par(expr, _))) , None) =>
              Some(expr.evaluate(env).toString)
            case st =>
              None
          }.filter(_.isDefined).map(_.get)


          destinationDescription.attributes.filterNot{
            f => changedFields.contains(f.name)
          }.foreach{column =>
            if(!column.isPrimaryKey) {
              source.id.description.fieldOption(column.name) match {
                case Some(a : Attribute) => {
                  if(a.dataType.getClass == column.dataType.getClass) {
                    d.update(column, source(a))
                  }
                }
                case _ => {}
              }
            }
          }
//          env.em.transaction{}

          one match {case AutoToOne() =>
            destinationDescription.ones.filter{one =>
              syncWhereDeclaration(env, one.entity).isDefined && Some(one) != parent.map(_.toOne) &&
                      !changedFields.contains(one.name) && one.source(d.id.dataSource).isDefined
            }.foreach{
              one => source.id.description.fieldOption(one.name) match {
                case Some(f : ToOne) => if(f.entity == one.entity) {
                  d.update(one, source(f) match {
                    case e : Entity => sync(env, e, one.entity, dataSourceExpression, options = SyncOptions(InsertOnly()))
                    case null => null
                  })
                }
                case _ => {}
              }
            }
          }
          many match {case a : AutoToMany =>
            destinationDescription.manies.filter{
              many => syncWhereDeclaration(env, many.entity).isDefined && !changedFields.contains(many.name) &&
                      many.one.source(d.id.dataSource).isDefined
            }.foreach{
              many => source.id.description.fieldOption(many.name) match {
                case Some(f : ToMany) => if(f.entity == many.entity && f.one == many.one) {
                  val b = collection.immutable.Set.newBuilder[Entity]
                  if(a.isInstanceOf[AutoToManyAppend]) {
                    b ++= d(many).asInstanceOf[Iterable[Entity]]
                  }
                  source(f) match {
                    case entities : Iterable[Entity] =>
                      entities.foreach{
                        e => b += sync(env, e, many.entity, dataSourceExpression,
                          options = SyncOptions(InsertOnly()),
                          parent = Some(ParentSync(d, many.one, found.isDefined)))
                      }
                    case null => {}
                  }
                  d.update(many, b.result())
                }
                case _ => {}
              }
            }
          }
        }
        case NoAutoUpdate() => {}
      }

      if(func.isDefined) {
        func.get.run(env, source , d)
      } else if(syncProc.isDefined) {
        val oldThisRef = env.thisRef
        env.setThisRef(Some(source))
        try {
          syncProc.get.value(env, Some(Seq(ParVal(d, None))), None)
        } finally {
          env.setThisRef(oldThisRef)
        }
      }


      env.setCurrentDataSource(oldDS)
    }
    d
  }
}

case class ParentSync(entity : Entity, toOne : ToOne, checkUnique : Boolean = true)