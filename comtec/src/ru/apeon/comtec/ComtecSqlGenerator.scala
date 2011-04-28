package ru.apeon.comtec

import ru.apeon.core.sql
import ru.apeon.core.eql._

class ComtecSqlGenerator extends SqlGenerator {
  def genVocUpdate(q: Insert) : Option[sql.Update] = q.from.entity.joinedTables match {
    case Seq() => None
    case Seq(join) => q.columns.filter{_.column.tableName(q.dataSource).getOrElse("") == join.table.name} match {
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
}