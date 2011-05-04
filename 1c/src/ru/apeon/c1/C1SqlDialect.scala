package ru.apeon.c1

import ru.apeon.core.sql.{DefaultSqlDialect}

class C1SqlDialect extends DefaultSqlDialect {
  override def appendEscape() {}
}