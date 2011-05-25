package ru.apeon.core.eql

import ru.apeon.core._
import entity._
import collection.mutable.{ListBuffer, HashMap}

/**
 * @author Anton Zherdev
 */

case class ToManySelect(toMany : ToMany, select : sql.Select) {
  override def toString = {
    select.toString
  }
}

object SqlGenerator {
  val generator : SqlGenerator = new SqlGenerator
  def apply(q : Select) : sql.Select = generator.gen(q)
  def apply(q : Insert) : Seq[sql.Statement] = generator.gen(q)
  def apply(q : Delete) : sql.Delete = generator.gen(q)
  def apply(q : Update) : Seq[sql.Update] = generator.gen(q)
  def apply(q : Expression) : sql.Expression = generator.genExpression(q, null)

  def generateToMany(sel : Select) : Seq[ToManySelect] = generator.generateToMany(sel)
  def generateToMany(toMany : ToMany) : ToManySelect = generator.generateToMany(toMany)
}

class SqlGenerator {
  def sqlTable(t : entity.Table) : sql.SqlTable = sql.SqlTable(t.schema, t.name)

  def default(q : Insert, table : Table) =
    q.from.entity.columnsWithColumn.filter{col =>
      q.columns.find(_.column == col).isEmpty &&
              col.default.isDefined &&
              col.tableName(q.dataSource).getOrElse(q.from.entity.table.name) == table.name
    }.map { col =>
      sql.InsertColumn(
        col.columnName(q.dataSource),
        col.default.get match {
          case DefaultInt(v) => sql.ConstNumeric(v)
          case DefaultString(s) => sql.ConstString(s)
          case _ => throw new RuntimeException("Not support")
        }
      )
    }

  def genMainInsert(q : Insert) = {
    var columns = q.columns.filter{
      _.column.tableName(q.dataSource).getOrElse(q.from.entity.table.name) == q.from.entity.table.name
    }.map{column =>
      sql.InsertColumn(column.column.columnName(q.dataSource), genExpression(column.expression, new EqlSqlFrom(q.dataSource)))
    }
    q.from.entity.discriminator match {
      case DiscriminatorColumn(column, value) =>
        columns = sql.InsertColumn(column, sql.Expression.constant(value)) +: columns
      case DiscriminatorNull() => {}
    }
    columns =  columns ++ default(q, q.from.entity.table)

    if(columns.isEmpty) {
      columns =  Seq(sql.InsertColumn(q.from.entity.primaryKeys.head.name, sql.ConstNull()))
    }

    sql.Insert(sqlTable(q.from.entity.table), columns)
  }

  def gen(q : Insert) : Seq[sql.Statement] = q.from.entity.joinedTables match {
    case Seq() =>
      Seq(genMainInsert(q))
    case _ => {
      genMainInsert(q) +:
      q.from.entity.joinedTables.map{join =>
        var columns =
          sql.InsertColumn(join.column, sql.Parameter("l_identity")) +: q.columns.filter{
            _.column.tableName(q.dataSource).getOrElse(q.from.entity.table.name) ==join.table.name
          }.map{column =>
            sql.InsertColumn(column.column.columnName(q.dataSource), genExpression(column.expression, new EqlSqlFrom(q.dataSource)))
          }
        columns = columns ++ default(q, join.table)
        sql.Insert(sqlTable(join.table), columns)
      }

    }
  }

  def gen(q : Update) : Seq[sql.Update] = {
    val ef = new EqlSqlFrom(q.dataSource)
    val e = q.from.entity
    e.joinedTables match {
      case Seq() => {
        ef.table = sql.FromTable(sqlTable(e.table), None)
        ef.append(q.from, ef.table, discriminator(ef, e, ef.table))

        val columns = q.columns.map{column =>
          sql.UpdateColumn(column.column.columnName(q.dataSource), genExpression(column.expression, ef))
        }

        var whereExpression : Option[sql.Expression] = None
        whereExpression = sql.And(whereExpression, genExpression(q.where, ef))
        whereExpression = sql.And(whereExpression, ef.where)

        Seq(sql.Update(ef.table.asInstanceOf[sql.FromTable], columns, whereExpression))
      }
      case _ => {
        genFrom(q.from, ef)
        var allWhere : Option[sql.Expression] = None
        allWhere = sql.And(allWhere, genExpression(q.where, ef))
        allWhere = sql.And(allWhere, ef.where)

        (JoinedTable(e.table, "") +: e.joinedTables).map{join =>
          val efIn = new EqlSqlFrom(q.dataSource)
          efIn.table = sql.FromTable(sqlTable(join.table), None)
          efIn.append(q.from, efIn.table, None)

          val columns = q.columns.filter(_.column.tableName(q.dataSource).getOrElse(e.table.name) == join.table.name).map{column =>
            sql.UpdateColumn(column.column.columnName(q.dataSource), genExpression(column.expression, efIn))
          }

          val eq = sql.Equal(
            sql.Ref(join.table.name, if(join.column.isEmpty) e.primaryKeys.head.name else join.column),
            sql.Ref("t", e.primaryKeys.head.name))

          sql.Update(efIn.gen.asInstanceOf[sql.FromTable], columns, Some(sql.Exists(ef.gen,sql.And(allWhere, Some(eq)))))
        }.filterNot(_.columns.isEmpty)
      }
    }
  }


  def gen(q : Delete) : sql.Delete = {
    val f : EqlSqlFrom = new EqlSqlFrom(q.dataSource)
    genFrom(q.from, f)

    var whereExpression : Option[sql.Expression] = None
    whereExpression = sql.And(whereExpression, genExpression(q.where, f))
    whereExpression = sql.And(whereExpression, f.where)

    sql.Delete(f.table.asInstanceOf[sql.FromTable], whereExpression)
  }



  def gen(q : Select) : sql.Select = q.columns match {
    case Seq() => gen(q, q.from.fields.filter{!_.isInstanceOf[ToMany]}.map{
      case col : Field =>
        Column(Dot(Ref(q.from), Ref(col)), col.name)
    })
    case _ => gen(q, q.columns)

  }

  def gen(q : Select, sel : Seq[Column]) : sql.Select = {
    val from : EqlSqlFrom = new EqlSqlFrom(q.dataSource)
    genFrom(q.from, from)
    val select = genColumns(sel, from)
    var whereExpression : Option[sql.Expression] = None
    whereExpression = sql.And(whereExpression, genExpression(q.where, from))
    whereExpression = sql.And(whereExpression, from.where)

    sql.Select(
      from.gen,
      select,
      whereExpression,
      q.orderBy.map{i => sql.OrderBy(genExpression(i.expression, from), i.direction match {
        case Asc() => sql.Asc()
        case Desc() => sql.Desc()
      })}
    )
  }

  def getSelectExpression(exp : ESelect , ef: EqlSqlFrom) : sql.ESelect = {
    val from : EqlSqlFrom = new EqlSqlFrom(ef.dataSource, Some(ef))
    genFrom(exp.from, from)

    var whereExpression : Option[sql.Expression] = None
    whereExpression = sql.And(whereExpression, genExpression(exp.where, from))
    whereExpression = sql.And(whereExpression, from.where)

    sql.ESelect(
      genExpression(exp.select, from),
      from.gen,
      whereExpression
    )
  }

  def getExistsExpression(exp : Exists , ef: EqlSqlFrom) : sql.Exists = {
    val from : EqlSqlFrom = new EqlSqlFrom(ef.dataSource, Some(ef))
    genFrom(exp.from, from)

    var whereExpression : Option[sql.Expression] = None
    whereExpression = sql.And(whereExpression, genExpression(exp.where, from))
    whereExpression = sql.And(whereExpression, from.where)

    sql.Exists(
      from.gen,
      whereExpression
    )
  }

  def discriminator(ef : EqlSqlFrom, entity : Description, table : sql.From) : Option[sql.Expression] = entity.discriminator match {
    case DiscriminatorColumn(column, value) => Some(sql.Equal(sql.Ref(Some(table.name), column), sql.Expression.constant(value)))
    case DiscriminatorNull() => None
  }

  def genTable(ef: EqlSqlFrom, e : Description) : sql.From = {
    val mainAlias = ef.alias
    var ret : sql.From = sql.FromTable(sqlTable(e.table), Some(mainAlias))
    e.joinedTables.foreach{join =>
      val a = ef.alias
      ret = ret.setLastJoin(
        sql.InnerJoin(sql.FromTable(sqlTable(join.table), Some(a)),
          sql.Equal(sql.Ref(a, join.column), sql.Ref(mainAlias, e.primaryKeys.head.columnName(ef.dataSource)))
        ))
    }
    ret
  }

  def genFrom(f : From, ef: EqlSqlFrom) : EqlSqlFrom = f match {
    case e : FromEntity =>{
      ef.table = genTable(ef, e.entity)
      ef.append(e, ef.table, discriminator(ef, e.entity, ef.table))
    }
    case e : FromToMany => {
      val t = e.ref match {
        case d : Dot => genFromTable(ef.parent.get, d.left, e.toMany.toOne.tableName(ef.dataSource))
        case r : Ref => ef.parent.get.get(r.defaultFrom, e.toMany.toOne.tableName(ef.dataSource) )
      }

      ef.table = genTable(ef, e.entity)
      ef.append(e, ef.table,
        Some(sql.And(
          discriminator(ef, e.entity, ef.table),
          sql.Equal(
            sql.Ref(t, e.toMany.entity.primaryKeys.head.columnName(ef.dataSource)),
            sql.Ref(ef.table, e.toMany.toOne.columnName(ef.dataSource))
          )
        )))
    }
    case _ => throw new SqlGeneratorError("Unknown type of table in main table")
  }

  def genColumns(columns : Seq[Column], ef: EqlSqlFrom) : Seq[sql.SelectColumn] =
    columns.filterNot(_.expression match {
      case ref : Dot => ref.right.isToMany
      case ref : Ref => ref.isToMany
      case _ => false
    }).map{column => column.expression match {
      case ref : Dot =>
        if(ref.right.isToOne) genToOne(column, getFrom(ef, Some(ref.left), ref.right), ref.right.declaration.asInstanceOf[ToOne], ef)
        else sql.Column(genExpression(ref, ef), Some(column.name))
      case ref : Ref =>
        if(ref.isToOne) genToOne(column, getFrom(ef, None, ref), ref.declaration.asInstanceOf[ToOne], ef)
        else sql.Column(genExpression(ref, ef), Some(column.name))
      case _ => sql.Column(genExpression(column.expression, ef), Some(column.name))
    }}

  def joinToOne(toOne : ToOne, ef: EqlSqlFrom, joinTo: sql.From) : sql.From = ef.getOrElse(toOne) {
    ef.join(toOne, genTable(ef, toOne.entity), sql.Ref(joinTo, toOne.columnName(ef.dataSource)))
  }

  def genToOne(col : eql.Column, from : sql.From, toOne : ToOne, ef: EqlSqlFrom) : sql.SelectColumn = {
    val ft = joinToOne(toOne, ef, from)

    sql.ColumnSeq(toOne.entity.fields.filter{!_.isInstanceOf[ToMany]}.map { col : Field => col match {
      case p : Attribute => sql.Column(sql.Ref(ef.find(ft, p.tableName(ef.dataSource)), p.columnName(ef.dataSource)), Some(p.name))
      case o : ToOne => sql.Column(sql.Ref(ef.find(ft, o.tableName(ef.dataSource)), o.columnName(ef.dataSource)), Some(o.name))
      case _ => throw new SqlGeneratorError("Select to many column")
    }}, toOne.name)
  }


  def genFromTable(ef : EqlSqlFrom, ft : eql.Expression, tableName : Option[String]) : sql.From = ft match {
    case ref : Dot => ref.right.declaration match {
      case toOne : ToOne => ef.find(joinToOne(toOne, ef, genFromTable(ef, ref.left, toOne.tableName(ef.dataSource))), tableName)
      case _ => throw SqlGeneratorError("Not to one")
    }
    case ref : Ref =>
      if(ref.isFrom) {
        ef.get(ref.from, tableName)
      }
      else {
        ref.declaration match {
          case toOne : ToOne => ef.find(joinToOne(toOne, ef, ef.get(ref.defaultFrom, toOne.tableName(ef.dataSource))), tableName)
          case _ => throw SqlGeneratorError("Not to one")
        }
      }
    case _ => throw SqlGeneratorError("Unknown expression")
  }

  def genExpression(exp : Option[Expression] , ef: EqlSqlFrom) : Option[sql.Expression] = exp match {
    case Some(e) => Some(genExpression(e, ef))
    case None => None
  }

  def getFrom(ef: EqlSqlFrom, left: Option[Expression], right: Ref): sql.From = {
    val tableName = right.declaration.asInstanceOf[FieldWithSource].tableName(ef.dataSource)
    val table: sql.From = left.map {
      l => genFromTable(ef, l, tableName)
    }.getOrElse(ef.get(right.defaultFrom, tableName))
    table
  }

  def genRef(ef: EqlSqlFrom, left : Option[Expression], right : Ref) : sql.Expression = right.declaration match {
    case p : Attribute => {
      if(p.isPrimaryKey) {
        left match {
          case Some(dot : Dot) => genRef(ef, Some(dot.left), dot.right)
          case _ =>  sql.Ref(getFrom(ef, left, right), p.columnName(ef.dataSource))
        }
      }
      else {
        sql.Ref(getFrom(ef, left, right), p.columnName(ef.dataSource))
      }
    }
    case o : ToOne => sql.Ref(getFrom(ef, left, right), o.columnName(ef.dataSource))
    case d : SqlGeneration => d.generateSql(genExpression(left.get, ef), right.parameters.map{
      par => genExpression(par, ef)
    })
    case null => throw SqlGeneratorError("Declaration is null")
    case o : Object => throw SqlGeneratorError("Unknown right class %s".format(o.getClass))
    case _ => throw SqlGeneratorError("Select to many column")
  }

  def genExpression(exp : Expression , ef: EqlSqlFrom) : sql.Expression =  exp match {
    case e : Equal => sql.Equal(genExpression(e.left, ef), genExpression(e.right, ef))
    case e : NotEqual => sql.NotEqual(genExpression(e.left, ef), genExpression(e.right, ef))
    case e : More => sql.More(genExpression(e.left, ef), genExpression(e.right, ef))
    case e : MoreOrEqual => sql.MoreOrEqual(genExpression(e.left, ef), genExpression(e.right, ef))
    case e : Less => sql.Less(genExpression(e.left, ef), genExpression(e.right, ef))
    case e : LessOrEqual => sql.LessOrEqual(genExpression(e.left, ef), genExpression(e.right, ef))
    case e : Like => sql.Like(genExpression(e.left, ef), genExpression(e.right, ef))
    case e : And => sql.And(genExpression(e.left, ef), genExpression(e.right, ef))
    case e : Or => sql.Or(genExpression(e.left, ef), genExpression(e.right, ef))
    case e : Plus => sql.Plus(genExpression(e.left, ef), genExpression(e.right, ef))
    case e : Minus => sql.Minus(genExpression(e.left, ef), genExpression(e.right, ef))
    case e : Mul => sql.Mul(genExpression(e.left, ef), genExpression(e.right, ef))
    case e : Div => sql.Div(genExpression(e.left, ef), genExpression(e.right, ef))
    case r : Dot => genRef(ef, Some(r.left), r.right)
    case r : Ref => genRef(ef, None, r)
    case n : ConstNumeric => sql.ConstNumeric(n.value)
    case n : ConstString => sql.ConstString(n.value)
    case n : ConstDate => sql.ConstDate(n.value)
    case e : Parameter => sql.Parameter(e.name)
    case f : SqlFunction => sql.Call(f.name, f.parameters.map{p => genExpression(p, ef)})
    case s : ESelect => getSelectExpression(s, ef)
    case s : AggPar1Function => sql.Call(s.name, Seq(genExpression(s.parameter, ef)))
    case n : ConstNull => sql.ConstNull()
    case not : Not => sql.Not(genExpression(not.expression, ef))
    case exists : Exists => getExistsExpression(exists, ef)
    case external : External => genExpression(external.eqlExpression, ef)
    case _ => throw new SqlGeneratorError("Unknown expression " + exp.getClass.getName)
  }




  class EqlSqlFrom(val dataSource : DataSource, val parent : Option[EqlSqlFrom] = None) {
    val tables = collection.mutable.Map[eql.From, sql.From]()
    val toOne : collection.mutable.Map[ToOne, sql.From] = new HashMap[ToOne, sql.From]
    var table : sql.From = _
    var joins : ListBuffer[sql.Join] = new ListBuffer[sql.Join]
    var where : Option[sql.Expression] = None

    var aliasCount = -1
    def alias = {
      aliasCount += 1
      prefix + "t" + (if(aliasCount == 0) "" else (aliasCount - 1).toString)
    }

    def append(eqlFrom : eql.From, sqlFrom : sql.From, where : Option[sql.Expression] = None) : EqlSqlFrom = {
      this.where = sql.And(this.where, where)
      tables += eqlFrom -> sqlFrom
      this
    }

    def join(to : ToOne, ft : sql.From, l : sql.Ref) : sql.From = {
      joins += sql.LeftJoin(ft, sql.And(sql.Equal(l, sql.Ref(ft, to.entity.primaryKeys.head.columnName(dataSource))),
        discriminator(this, to.entity, ft)) )
      toOne += to -> ft
      ft
    }

    def getOrElse(ref : ToOne) (f : => sql.From) : sql.From = {
      toOne.getOrElse(ref, f)
    }

    def find(f : sql.From, tableName : Option[String] ) : sql.From  = tableName match {
      case None => f
      case Some(name) =>
        f.froms.find(_.asInstanceOf[sql.FromTable].table.name == name).get
    }

    def get(ft : eql.From, tableName : Option[String]) : sql.From = {
      find(tables.get(ft).getOrElse{parent.get.get(ft, tableName)}, tableName)
    }

    def prefix : String = parent match {
      case Some(p) => "s_" + p.prefix
      case None => ""
    }

    def gen : sql.From = {
      var ret = table
      joins.foreach{join =>
        ret = ret.setLastJoin(join)
      }
      ret
    }

  }

  def generateToMany(sel : Select) : Seq[ToManySelect] = sel.columns.filter{c : Column => c.expression match {
    case d : Dot => d.right.isToMany
    case r : Ref => r.isToMany
    case _=> false
  }}.map{c => c.expression match {
    case d : Dot => generateToMany(d.right.declaration.asInstanceOf[ToMany])
    case r : Ref => generateToMany(r.declaration.asInstanceOf[ToMany])
  }
  }

  def generateToMany(toMany : ToMany) : ToManySelect = {
    val ft = new FromEntity(toMany.entity, None)
    new ToManySelect(toMany, SqlGenerator(
      Select(
        columns = toMany.entity.fields.map{ column : Field =>
          if(column == toMany.toOne)
            Column(Dot(Dot(Ref(ft), Ref(toMany.toOne)), Ref(Id)), column.name)
          else
            Column(Dot(Ref(ft), Ref(column)), column.name)
        },
        from = ft,
        where = Some(
          Equal(Dot(Ref(ft), Ref(toMany.toOne)), Parameter("id") )
        ))
    ))
  }
}

case class SqlGeneratorError(s : String) extends Exception(s)

trait SqlGeneration{
  def generateSql(ref : sql.Expression, parameters : Seq[sql.Expression]) : sql.Expression
}