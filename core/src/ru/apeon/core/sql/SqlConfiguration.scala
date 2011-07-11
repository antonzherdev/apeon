package ru.apeon.core.sql

import javax.naming.{Context, InitialContext}

/**
 * @author Anton Zherdev
 */

object SqlConfiguration {
  var dialect : SqlDialect = null

  var dataSource : DataSource = null
}

trait SqlConfiguration {
  def apply()
}

object DefaultSqlConfiguration extends SqlConfiguration {
  def apply() {
    SqlConfiguration.dialect = new DefaultSqlDialect
    SqlConfiguration.dataSource = {
      new DataSource((new InitialContext).lookup("java:comp/env") match {
        case envContext : Context => envContext.lookup("datasource/apeon") match {
          case ds : javax.sql.DataSource => ds
          case _ => throw new RuntimeException("Could not get datasource")
        }
        case _ => throw new RuntimeException("Could not get enviroment context")
      }, SqlConfiguration.dialect)
    }
  }
}
