package ru.apeon.core.sql

import util.logging.ConsoleLogger

trait SqlDialect extends ConsoleLogger {
  val schema : String = ""
  val catalog : String = null

  //TODO: Колонка id не является обязательной. Нужно добавлять таблицу без колонки id. Может быть, добавлять пустую колонку или выполнять проверку только с одной уже известной колонкий. Добавлять pk нужно уже при добавлении соответствующей колонки.
  def createTableStatement(table : SqlTable) : String =
    """create table %s(id integer primary key default autoincrement)""".format(table)

  def createColumnStatement(column : SqlColumn) =
    """alter table %s add %s;""".format(column.table, columnDefinitionStatement(column))

  def modifyColumnStatement(column : SqlColumn) =
    """alter table %1$s modify %2$s;alter table %1$s modify "%3$s" %4$s;%5$s""".format(
      column.table, columnDefinitionStatement(column),
      column.name, if(column.nulls) "null" else "not null",
      column.default match {
        case Some(s) => ""
        case None => """alter table %s alter "%s" drop default""".format(column.table, column.name)
      }
    )

  def columnDefinitionStatement(column : SqlColumn) =
    "\"" + column.name + "\" " +
            column.dataType + " " +
            (if(column.nulls) "null" else "not null") +
            (if(column.default.isDefined) " default " + column.default.get else "")



  def createForeignKeyStatement(key : SqlForeignKey) =
"""alter table %s add foreign key "%s"(%s) references %s(%s) on update %s on delete %s""".format(
key.foreign.table,
key.name,
key.foreign.columnQuote,
key.primary.table,
key.primary.columnQuote,
key.onUpdate,
key.onDelete
)

  def lastIdentityExpression(table : SqlTable) = "@@identity"
}

class DefaultSqlDialect extends SqlDialect

class AsaSqlDialect extends SqlDialect {
  override val schema = "dba"

  override def createTableStatement(table: SqlTable) =
"""create table %1$s(id integer default 0 primary key);
insert into %1$s(id) values(0);""".format(table)
}

