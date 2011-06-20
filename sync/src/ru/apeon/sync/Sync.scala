package ru.apeon.sync

import ru.apeon.core.entity._
import ru.apeon.core.eql
import ru.apeon.core.script._
import scala.PartialFunction

object Sync {
  abstract class SyncWhere {
    def eqlValue : eql.Expression
    def sourceAlias : String
    def destinationAlias : String
    def sourceDescription : Description
    def destinationDescription : Description

    lazy val buildEql : eql.Expression = {
      abstract class F { val field : Field}
      case class DestinationField(field : Field) extends F
      case class SourceField(field : Field) extends F
      def field(e: eql.Dot): F = {
        e.left match {
          case eql.Ref(name) if name == sourceAlias => SourceField(sourceDescription.field(e.right.name))
          case eql.Ref(name) if name == destinationAlias => DestinationField(destinationDescription.field(e.right.name))
          case d : eql.Dot => field(d) match {
            case DestinationField(o : ToOne) => DestinationField(o.entity.field(e.right.name))
            case SourceField(o : ToOne) => SourceField(o.entity.field(e.right.name))
            case _ => throw ScriptException("Not to one.")
          }
        }
      }
      eqlValue.map {
        case e @ eql.Equal(left : eql.Dot, right : eql.Dot) => {
          val fields = (field(left), field(right)) match {
            case f @ (SourceField(l), DestinationField(r)) => (l, r, left, right)
            case (DestinationField(l), SourceField(r)) => (r, l, right, left)
          }
          fields match {
            case (l : ToOne, r : ToOne, s, d) => {
              if(l != r) throw SyncException("Right to one not equals left to one. Use primary key to compare.")
              syncWhere(new DefaultEnvironment(), l.entity, r.entity).eqlValue.map{
                case eql.Dot(eql.Ref(name), v) if name == sourceAlias => eql.Dot(s, v)
                case eql.Dot(eql.Ref(name), v) if name == destinationAlias => eql.Dot(d, v)
              }
            }
            case _ => e
          }
        }
      }
    }

    def replaceRef(source : Entity) : eql.Expression = buildEql.map {
      case ref: eql.Dot => replaceRef(source, sourceAlias, ref) match {
        case Some(a) => eql.Const(a)
        case None => ref
      }
    }

    private def replaceRef(source : Entity, sourceAlias : String, dot : eql.Dot) : Option[Any] = dot.left match {
      case parent : eql.Dot => replaceRef(source, sourceAlias, parent) match {
        case Some(e : Entity) => Some(e(dot.right.name))
        case Some(null) => Some(null)
        case _ => None
      }
      case r : eql.Ref => if(r.name == sourceAlias) Some(source(dot.right.name)) else None
      case _ => None
    }

    def buildFields : Seq[Seq[String]]
  }
  case class SyncWhereEql(eqlValue : eql.Expression,
                          sourceDescription : Description, sourceAlias : String,
                          destinationDescription : Description, destinationAlias : String) extends SyncWhere
  {
    def buildFields = throw SyncException("Not supported with eql where")
    override def hashCode = eqlValue.hashCode()
  }

  case class SyncWhereFields(fields : Seq[String],
                             sourceDescription : Description, destinationDescription : Description) extends SyncWhere
  {
    def eqlValue = {
      val ands = fields.map{field => eql.Equal(eql.Dot("d." + field),  eql.Dot("s." + field))}
      var ret : eql.Expression = ands.head
      for(and <- ands.tail) {
        ret = eql.And(ret, and)
      }
      ret
    }
    val sourceAlias = "s"
    val destinationAlias = "d"

    lazy val buildFields = {
      val b = Seq.newBuilder[Seq[String]]
      for(field <- fields) {
        sourceDescription.field(field) match {
          case a : Attribute => b += Seq(field)
          case o : ToOne => b ++= syncWhere(new DefaultEnvironment(), o.entity, o.entity).buildFields.map{
            s => field +: s
          }
        }
      }
      b.result()
    }

    override def hashCode = fields.hashCode()
  }


  private def syncWhereDeclaration(env: Environment, destinationDescription: Description): Option[Declaration] =
    destinationDescription.declarations.find {
      dec => dec.name == "syncWhere" && dec.isInstanceOf[Def] && dec.asInstanceOf[Def].parameters.isEmpty
    }


  private def syncWhere(env: Environment, sourceDescription: Description, destinationDescription : Description): SyncWhere = {
    if(sourceDescription != destinationDescription) throw SyncException("Source and destination descriptions are not equal")
    syncWhereDeclaration(env, sourceDescription).getOrElse {
      throw SyncException("Sync where is not found")
    }.value(env, None, None) match {
      case e : eql.Expression => SyncWhereEql(e, sourceDescription, "s", destinationDescription, "d")
      case fields : Seq[String] => SyncWhereFields(fields, sourceDescription, destinationDescription)
    }
  }

  private def syncWhere(env: Environment,
                        sourceDescription: Description, sourceAlias : String,
                        destinationDescription: Description,  destinationAlias : String,
                        where: Option[eql.Expression]) : SyncWhere =
  {
    where.map(e => SyncWhereEql(e, sourceDescription, sourceAlias, destinationDescription, destinationAlias)).getOrElse{
      syncWhere(env, sourceDescription, destinationDescription)
    }
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
               options : SyncOptions = SyncOptions(),
               aliases: (String, String) = ("s", "d") ) : Option[Entity] =
  {
    syncFindW(env, source, destinationDescription, dataSourceExpression,
      syncWhere(env, source.id.description, aliases._1, destinationDescription, aliases._2, where),
      parent, options, aliases
    )
  }

  private def syncFindW(env: Environment, source: Entity, destinationDescription: Description,
                        dataSourceExpression: Option[Expression],
                        where: SyncWhere, parent: Option[ParentSync] = None,
                        options : SyncOptions = SyncOptions(),
                        aliases: (String, String) = ("s", "d")) : Option[Entity] =
  {
    env.cache.getOrElseUpdate((17, source.id, where).hashCode(), {
      val ds = dataSource(env, destinationDescription, dataSourceExpression, parent)
      if(options.hasOptimization(HashIndexOptimization())) {
        val fields = where.buildFields
        env.cache.getOrElseUpdate((19, ds, destinationDescription, where).hashCode(), {
          env.em.select(eql.Select(eql.FromEntity(destinationDescription, None, eql.DataSourceExpressionDataSource(ds)))).map {
            e => (e.hashCode(fields), e)
          }.toMap
        }).asInstanceOf[Map[Int, Entity]].get(source.hashCode(fields))
      } else {
        var w: eql.Expression = where.replaceRef(source)
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
              throw SyncException(
                """Many entities to sync.
              Source datasource = %s
              Source = %s

              Destination datasource = %s
              Destination = %s""".format(source.id.dataSource.fullName, source, ds.fullName, many))
          }
        } else {
          None
        }
      }
    }).asInstanceOf[Option[Entity]]
  }

  def syncAny(env : Environment, source : Any, destinationDescription : Description,
              dataSourceExpression : Option[Expression], where : Option[eql.Expression] = None,
              func : Option[BuiltInFunction] = None, parent : Option[ParentSync] = None,
              options : SyncOptions = SyncOptions()) : Any = source match
  {
    case s : Traversable[Entity] => syncMany(env, s, destinationDescription, dataSourceExpression, where, func, parent, options)
    case s : Entity => sync(env, s, destinationDescription, dataSourceExpression, where, func, parent, options)
    case null => null
  }

  def syncMany(env : Environment, sources : Traversable[Entity], destinationDescription : Description,
               dataSourceExpression : Option[Expression], where : Option[eql.Expression] = None,
               func : Option[BuiltInFunction] = None, parent : Option[ParentSync] = None,
               options : SyncOptions = SyncOptions()) : Traversable[Entity] =
  {
    if(sources.isEmpty) return Seq()

    val opt = if(sources.size > 100) options.addOptimization(HashIndexOptimization()) else options
    val aliases: (String, String) = getAliases(func)
    val w = syncWhere(env, sources.head.id.description, aliases._1, destinationDescription, aliases._2, where)
    sources.map{
      source => syncW(env, source, destinationDescription, dataSourceExpression, w, func, parent, opt, aliases)
    }
  }

  def sync(env : Environment, source : Entity, destinationDescription : Description,
           dataSourceExpression : Option[Expression], where : Option[eql.Expression] = None,
           func : Option[BuiltInFunction] = None, parent : Option[ParentSync] = None,
           options : SyncOptions = SyncOptions()) : Entity =
  {
    val aliases: (String, String) = getAliases(func)
    syncW(env, source, destinationDescription, dataSourceExpression,
      syncWhere(env, source.id.description, aliases._1, destinationDescription, aliases._2, where),
      func, parent, options, aliases
    )
  }

  private def getAliases(func: Option[BuiltInFunction]): (String, String) = {
    func.map {
      f =>
        val pars = f.parameters
        (pars(0).name, pars(1).name)
    }.getOrElse(("s", "d"))
  }

  private def syncAttributes(destDesc: Description, source: Entity, dest: Entity, changedFields: Seq[String]) {
    destDesc.attributes.filterNot {
      f => changedFields.contains(f.name)
    }.foreach {
      column =>
        if (!column.isPrimaryKey) {
          source.id.description.fieldOption(column.name) match {
            case Some(a: Attribute) => {
              if (a.dataType.getClass == column.dataType.getClass) {
                dest.update(column, source(a))
              }
            }
            case _ => {}
          }
        }
    }
  }

  private def syncOne(env: Environment, destDesc: Description, parent: Option[ParentSync], source: Entity,
                      dest: Entity, oneMode: AutoToOneMode, dsExp: Option[Expression], changedFields: scala.Seq[String])
  {
    oneMode match {
      case AutoToOne() =>
        destDesc.ones.filter {
          one =>
            syncWhereDeclaration(env, one.entity).isDefined && Some(one) != parent.map(_.toOne) &&
                    !changedFields.contains(one.name) && one.source(dest.id.dataSource).isDefined
        }.foreach {
          one => source.id.description.fieldOption(one.name) match {
            case Some(f: ToOne) => if (f.entity == one.entity) {
              try {
                dest.update(one, source(f) match {
                  case e: Entity => sync(env, e, one.entity, dsExp, options = SyncOptions(InsertOnly()))
                  case null => null
                })
              } catch tt (one.name)
            }
            case _ => {}
          }
        }
    }
  }

  private def syncMany(env: Environment, destDesc: Description, d: Entity, source: Entity,
                       manyMode: AutoToManyMode, dsExp: Option[Expression], isFound: Boolean,
                       changedFields: scala.Seq[String])
  {
    manyMode match {
      case a: AutoToMany =>
        destDesc.manies.filter {
          many => syncWhereDeclaration(env, many.entity).isDefined && !changedFields.contains(many.name) &&
                  many.one.source(d.id.dataSource).isDefined
        }.foreach {
          many => source.id.description.fieldOption(many.name) match {
            case Some(f: ToMany) => if (f.entity == many.entity && f.one == many.one) {
              val b = collection.immutable.Set.newBuilder[Entity]
              if (a.isInstanceOf[AutoToManyAppend]) {
                b ++= d(many).asInstanceOf[Iterable[Entity]]
              }
              source(f) match {
                case entities: Iterable[Entity] =>
                  try {
                    entities.foreach {
                      e => b += sync(env, e, many.entity, dsExp,
                        options = SyncOptions(InsertOnly()),
                        parent = Some(ParentSync(d, many.one, isFound)))
                    }
                  } catch tt(many.name)
                case null => {}
              }
              d.update(many, b.result())
            }
            case _ => {}
          }
        }
    }
  }

  private def syncW(env : Environment, source : Entity, destDesc : Description,
                    dsExp : Option[Expression], where : SyncWhere,
                    func : Option[BuiltInFunction] = None, parent : Option[ParentSync] = None,
                    options : SyncOptions = SyncOptions(), aliases: (String, String) = ("s", "d")) : Entity =
  {

    val found = syncFindW(env, source, destDesc, dsExp, where, parent, options, aliases)
    if(found.isEmpty && UpdateOnly() == options.sync) {
      return null
    }
    val dest : Entity = found.getOrElse{
      val ds: DataSource = dataSource(env, destDesc, dsExp, parent)
      val ret = env.em.insert(destDesc, ds)
      env.cache.update((17, source.id, where).hashCode(), Some(ret))
      ret
    }

    if(parent.isDefined) {
      dest.update(parent.get.toOne, parent.get.entity)
    }

    var funcDestinationAlias = aliases._2
    val syncProc : Option[Def] = if(func.isDefined) None else {
      destDesc.declarations.find{
        decl => decl.name == "syncProc" && (decl.parameters match {
          case Seq(DefPar(name, ScriptDataTypeEntity(descr))) => if(descr == destDesc) {
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
      env.setCurrentDataSource(Some(dest.id.dataSource))

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


          syncAttributes(destDesc, source, dest, changedFields)
          syncOne(env, destDesc, parent, source, dest, one, dsExp, changedFields)
          syncMany(env, destDesc, dest, source, many, dsExp, found.isDefined, changedFields)
        }
        case NoAutoUpdate() => {}
      }

      if(func.isDefined) {
        func.get.run(env, source , dest)
      } else if(syncProc.isDefined) {
        val oldThisRef = env.thisRef
        env.setThisRef(Some(source))
        try {
          syncProc.get.value(env, Some(Seq(ParVal(dest, None))), None)
        } finally {
          env.setThisRef(oldThisRef)
        }
      }


      env.setCurrentDataSource(oldDS)
    }
    dest
  }

  def tt(column : String) : PartialFunction[Throwable, Any] = {
    case e @ SyncException(message, s, path) => throw SyncException(message, s.orElse(Some(e)), column +: path)
    case t : Throwable => throw SyncException(t.getMessage, Some(t), Seq(column))
  }
}

case class ParentSync(entity : Entity, toOne : ToOne, checkUnique : Boolean = true)

case class SyncException(message : String,
                         cause: Option[Throwable] = None,
                         path : Seq[String] = Seq()) extends RuntimeException(message, cause.getOrElse{null}) {
  override def getMessage = path match {
    case Seq() => message
    case _ => "%s: %s ".format(path.mkString("/"), message)
  }
}