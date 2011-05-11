package ru.apeon.c1

import ru.apeon.core.sql._

class C1SqlDialect extends DefaultSqlDialect {
  override def appendEscape() {}

  override protected val stringSymbol = '"'
  override protected val escapeStringSymbol = '\\'
}