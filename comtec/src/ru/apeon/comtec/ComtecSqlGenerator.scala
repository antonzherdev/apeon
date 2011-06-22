package ru.apeon.comtec

import ru.apeon.core.sql
import ru.apeon.core.eql._
import ru.apeon.core.entity.{FieldWithSource, Description}

class ComtecSqlGenerator extends SqlGenerator {
  def genVocUpdate(q: Insert) : Option[sql.Update] = q.from.entity.joinedTables match {
    case Seq() => None
    case Seq(join) => q.columns.filterNot(_.column.isNullFor(q.dataSource)).filter{_.column.tableName(q.dataSource).getOrElse("") == join.table.name} match {
      case Seq() => None
      case columns => Some(sql.Update(sql.FromTable(sqlTable(join.table)),
        columns.map{column =>
          sql.UpdateColumn(column.column.columnName(q.dataSource), genExpression(column.expression, new EqlSqlFrom(q.dataSource)))
        },
        Some(sql.Equal(sql.Ref(join.column), sql.Parameter("l_identity")))
      ))
    }
  }

  override def gen(q: Insert) : Seq[sql.Statement] = q.from.entity.table.name match  {
    case "voc_names" => genVocUpdate(q) match {
      case Some(update) => Seq(genMainInsert(q), update)
      case None => Seq(genMainInsert(q))
    }
    case _ => super.gen(q)
  }

  def nonNull(ef : EqlSqlFrom, entity: Description, table: sql.From, pk : FieldWithSource) =
    sql.NotEqual(sql.Ref(Some(table.name), pk.columnName(ef.dataSource)), sql.Expression.constant(0))

  override def discriminator(ef : EqlSqlFrom, entity: Description, table: sql.From) = entity.primaryKeys match {
    case Seq(pk) => super.discriminator(ef, entity, table) match {
      case Some(d) => Some(sql.And(nonNull(ef, entity, table, pk), d))
      case None => Some(nonNull(ef, entity, table, pk))
    }
    case _ => super.discriminator(ef, entity, table)
  }

}