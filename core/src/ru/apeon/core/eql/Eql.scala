package ru.apeon.core.eql

import collection._
import ru.apeon.core._
import sql.SqlTable

/**
 * @author Anton Zherdev
 */

trait Eql extends sql.Sql {
  def select(statement : Select) : sql.Rows = select(statement, Map[String, Any]())

  def select(statement : Select, parameters : collection.Map[String, Any]) : sql.Rows =
    new EqlRows(this, statement, select(SqlGenerator(statement), parameters))

  def delete(delete : Delete, parameters : collection.Map[String, Any] = Map()) {
    execute(SqlGenerator(delete), parameters)
  }

  def update(update : Update, parameters : collection.Map[String, Any] = Map()) {
    SqlGenerator(update).foreach {
      u =>
        execute(u, parameters)
    }
  }

  def insert(insert : Insert, parameters : collection.Map[String, Any] = Map()) : Int = {
    val inserts = SqlGenerator(insert)
    execute(inserts.head, parameters)
    val id = lastIdentity(new SqlTable(insert.from.entity.table.schema, insert.from.entity.table.name))
    val pars = parameters + ("l_identity" -> id)
    inserts.tail.foreach(ins => execute(ins, pars))
    id
  }

  def selectOne(statement : Select, parameters : collection.Map[String, Any] = Map()) : Option[sql.Row] = {
    selectOne(SqlGenerator(statement), parameters) match {
      case Some(row) => Some(new EqlRow(this, statement, row))
      case None => None
    }
  }
}

class EqlRows(val eql : Eql, val select : Select, parent : sql.Rows) extends sql.RowsDecorator(parent) {
  override def toSeqMutableMap : Seq[mutable.Map[String, Any]] = {
    val par : Seq[mutable.Map[String, Any]]  = super.toSeqMutableMap
    val selects = SqlGenerator.generateToMany(select)
    if(selects.isEmpty) return par;
    selects.foreach { sel : ToManySelect =>
      par.foreach{row =>
        row.update(sel.toMany.name, eql.select(sel.select, immutable.Map("id" -> row("id"))).toSeqMutableMap)
      }
    }

    par
  }
}

class EqlRow(val eql : Eql, val select : Select, parent : sql.Row) extends sql.RowDecorator(parent) {
  override def toMutableMap : mutable.Map[String, Any] = {
    val row : mutable.Map[String, Any]  = super.toMutableMap
    val selects = SqlGenerator.generateToMany(select)
    selects.foreach { sel : ToManySelect =>
      row.update(sel.toMany.name, eql.select(sel.select, immutable.Map("id" -> row("id"))).toSeqMutableMap)
    }

    row
  }
}