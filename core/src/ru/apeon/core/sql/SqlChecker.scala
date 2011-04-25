package ru.apeon.core.sql

import java.sql.{DatabaseMetaData, ResultSetMetaData, Connection}
import akka.util.Logging

class SqlChecker(val dialect : SqlDialect = SqlConfiguration.dialect) extends Logging {
  def checkTable(con : Connection, table : SqlTable) =
      if(!isTableExist(con, table)) {
        createTable(con, table)
      }

  /**
   * Проверяет существование таблицы
   * @param con соединение
   * @param table таблица
   */
  protected def isTableExist(con : Connection, table : SqlTable) =
    con.getMetaData.getTables(null, table.schema, table.name, null).next

  /**
   * Создает таблицу
   * @param con соединение
   * @param table таблица
   */
  protected def createTable(con : Connection, table : SqlTable) = {
    val sql = dialect.createTableStatement(table)
    log.info("Create table:\n%s".format(sql))
    con.createStatement.execute(sql)
  }

  protected def loadRealColumn(con : Connection, column : SqlColumn) : Option[SqlColumn] = {
    val rs = con.getMetaData.getColumns(null, column.table.schema, column.table.name, column.name)
    if(rs.next) {
      Some(new SqlColumn(column.table, column.name,
        (rs.getString("TYPE_NAME") match {
          case s if s == "decimal" || s == "numeric" =>
            new SqlDataType(s, Some(rs.getInt("COLUMN_SIZE")), Some(rs.getInt("DECIMAL_DIGITS")))
          case s if s == "char" || s == "varchar" =>
             new SqlDataType(s, Some(rs.getInt("COLUMN_SIZE")), None)
          case s =>
            new SqlDataType(s, None, None)
        }),
        (rs.getInt("NULLABLE") match{
          case ResultSetMetaData.columnNoNulls => false
          case ResultSetMetaData.columnNullable => true
          case _ => true
        }),
        (rs.getString("COLUMN_DEF") match {
          case null => None
          case s => Some(s)
        })
      ))
    }
    else {
      None
    }
  }

  /**
   * Проверяет колонку на соответствие ее описанию и модифицирует или создает колонку, если это необходимо
   * @param con соединение
   * @param fieldOption описание колонки
   */
  def checkColumn(con : Connection, column : SqlColumn) {
    loadRealColumn(con, column) match {
      case Some(real) => {
        if(real != column) {
          modifyColumn(con, column)
        }
      }
      case None => createColumn(con, column)
    }
  }

  /**
   * Создает новую колонку
   * @param con соединение
   * @param fieldOption описание колонки
   */
  def createColumn(con : Connection, column : SqlColumn) {
    val sql = dialect.createColumnStatement(column)
    log.info("Create column:\n%s".format(sql))
    con.createStatement.execute(sql)
  }


  /**
   * Модифицирует существующую колонку
   * @param con соединение
   * @param fieldOption описание колонки
   */
  def modifyColumn(con : Connection, column : SqlColumn) {
    val sql = dialect.modifyColumnStatement(column)
    log.info("Modify column:\n%s".format(sql))
    con.createStatement.execute(sql)
  }

  def fkAction(num :Int) = num match {
    case DatabaseMetaData.importedKeyNoAction => SqlForeignKeyActionRestrict()
    case DatabaseMetaData.importedKeyRestrict => SqlForeignKeyActionRestrict()
    case DatabaseMetaData.importedKeyCascade => SqlForeignKeyActionCascade()
    case DatabaseMetaData.importedKeySetDefault => SqlForeignKeyActionSetDefault()
    case DatabaseMetaData.importedKeySetNull => SqlForeignKeyActionSetNull()
  }

  protected def loadRealForeignKey(con : Connection, key : SqlForeignKey) : Option[SqlForeignKey] = {
    val rs = con.getMetaData.getExportedKeys(null, key.primary.table.schema, key.primary.table.name)
    var find : Boolean = false
    while(!find && rs.next) {
      if(rs.getString("FKTABLE_NAME").equalsIgnoreCase(key.foreign.table.name) &&
        rs.getString("FKCOLUMN_NAME").equalsIgnoreCase(key.foreign.column) ) {
          find = true
        }
    }
    if(find) {
      Some(SqlForeignKey(
        name = rs.getString("FK_NAME"),
        primary = SqlReference(SqlTable(rs.getString("PKTABLE_SCHEM"), rs.getString("PKTABLE_NAME")), Seq(rs.getString("PKCOLUMN_NAME"))),
        foreign = SqlReference(SqlTable(rs.getString("FKTABLE_SCHEM"), rs.getString("FKTABLE_NAME")), Seq(rs.getString("FKCOLUMN_NAME"))),
        onUpdate = fkAction(rs.getInt("UPDATE_RULE")), onDelete = fkAction(rs.getInt("DELETE_RULE"))
      ))
    }
    else {
      None
    }
  }

  //TODO: Сделать проверку не только на наличие FK, но и на соответствие параметров
   /**
   * Проверить существование внешнего ключа
   */
  def checkForeignKey(con : Connection, key : SqlForeignKey) {
    val fk = loadRealForeignKey(con, key)
    if(fk.isEmpty) {
      createForeignKey(con, key)
    }
  }

  def createForeignKey(con : Connection, key : SqlForeignKey) {
    val sql = dialect.createForeignKeyStatement(key)
    log.debug(sql)
    con.createStatement.execute(sql)
  }
}