package ru.apeon.c1

import ru.apeon.core.eql.{Expression, SqlGenerator}
import ru.apeon.core.sql

/**
 * @author Anton Zherdev
 */

class C1SqlGenerator extends SqlGenerator {
  override def genExpression(exp : Expression , ef: EqlSqlFrom) : sql.Expression = exp match {
    case ref : ConstRef => sql.Parameter("ref")
    case _ => super.genExpression(exp, ef)
  }
}