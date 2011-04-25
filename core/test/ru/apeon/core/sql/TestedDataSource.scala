package ru.apeon.core.sql

import java.lang.{Class, String}
import java.io.{Reader, InputStream, PrintWriter}
import java.math.BigDecimal
import java.util.{Calendar, Map, Properties}
import collection.SortedMap
import java.sql._
import collection.mutable.ArrayBuffer

/**
 * @author Anton Zherdev
 */

class TestedDataSource extends DataSource(null) {
  val con : TestedConnection = new TestedConnection(this)


  override val checker = new TestedChecked

  override def unwrap[T](iface: Class[T]) = throw new RuntimeException("unwrap")

  override def setLogWriter(out: PrintWriter) = null

  override def setLoginTimeout(seconds: Int) = null

  override def isWrapperFor(iface: Class[_]) = false

  override def getLogWriter = null

  override def getLoginTimeout = 0

  override def getConnection = con

  override def getConnection(userName: String, password: String) = con

  def addResult(f : PartialFunction[String, TestedResultSet]) : Unit = results += f

  def addResultMap(f : PartialFunction[String, Seq[SortedMap[String, _]]]) : Unit = results += {
    case s if f.isDefinedAt(s) => toTestedResultSet(f(s))
  }


  def withResult(columns : Seq[String], rows : Seq[Seq[_]])( f : => Unit ) : Unit =
    withResult(new TestedResultSet(new TestedResultSetMetaData(columns.map{column => new TestedColumn(column)}), rows))(f)

  def withResult(rs : TestedResultSet) ( f : => Unit ) : Unit = {
    results.clear
    addResult({case _ => rs})
    f
    results.clear
  }

  def withResult(pf : PartialFunction[String, TestedResultSet]) ( f : => Unit ) : Unit = {
    results.clear
    addResult(pf)
    f
    results.clear
  }

  val results : ArrayBuffer[PartialFunction[String, TestedResultSet]]  = ArrayBuffer.empty

  def addTable(name : String) : Unit = addTable(SqlTable("", name))
  def addTable(table : SqlTable) : Unit = tables += table
  val tables : ArrayBuffer[SqlTable]  = ArrayBuffer.empty

  def addColumn(column : SqlColumn) = columns += column
  val columns : ArrayBuffer[SqlColumn] = ArrayBuffer.empty
  def findColumn(table : SqlTable, column : String) =
    columns.find(c => c.table == table && c.name == column)

  def addForeignKey(key : SqlForeignKey) = foreignKeys += key
  val foreignKeys : ArrayBuffer[SqlForeignKey] = ArrayBuffer.empty


  private def toTestedResultSet(rows : Seq[SortedMap[String, _]]) = {
    val newColumns = ArrayBuffer[TestedColumn]()
    val newRows = ArrayBuffer[Seq[_]]()
    rows.foreach{row =>
      row.foreach{column =>
        val pos = newColumns.indexOf(column._1)
        if(newColumns.find(_.name == column._1).isEmpty) {
          newColumns.append(new TestedColumn(column._1))
        }
      }
    }

    rows.foreach{row =>
      val newRow = ArrayBuffer[Any]()
      newColumns.foreach{column =>
        newRow.append(null)
      }
      var i = 0
      newColumns.foreach{column =>
        if(row.isDefinedAt(column.name)) {
          newRow.update(i, row(column.name))
        }
        i += 1
      }
      newRows.append(newRow)
    }

    new TestedResultSet(new TestedResultSetMetaData(newColumns), newRows)
  }
}

class TestedChecked extends SqlChecker {
  override def isTableExist(con: Connection, table: SqlTable) = con.asInstanceOf[TestedConnection].ds.tables.contains(table)

  override def createTable(con: Connection, table: SqlTable) =
    if(con.asInstanceOf[TestedConnection].ds.tables.contains(table)) {
      throw new RuntimeException("Table \"%s\" already exists.".format(table))
    }
    else{
      con.asInstanceOf[TestedConnection].ds.tables += table
      true
    }


  override def loadRealColumn(con: Connection, column: SqlColumn) =
    con.asInstanceOf[TestedConnection].ds.columns.find{ c => (c.table == column.table) && (c.name == column.name)}

  override def createColumn(con: Connection, column: SqlColumn) =
    con.asInstanceOf[TestedConnection].ds.columns += column

  override def modifyColumn(con: Connection, column: SqlColumn) = {
    val columns = con.asInstanceOf[TestedConnection].ds.columns
    val toDelete = loadRealColumn(con, column)
    if(toDelete.isDefined) {
      columns -= toDelete.get
    }
    else {
       throw new RuntimeException("Column \"%s\" not exists in table \"%s\".".format(column.name, column.table))
    }
    columns += column
  }

  override protected def loadRealForeignKey(con: Connection, key: SqlForeignKey) =
     con.asInstanceOf[TestedConnection].ds.foreignKeys.find{ k => k.foreign == key.foreign }

  override def createForeignKey(con: Connection, key: SqlForeignKey) = {
    con.asInstanceOf[TestedConnection].ds.foreignKeys += key
  }
}

class TestedConnection(val ds : TestedDataSource) extends Connection{
  def unwrap[T](iface: Class[T]) = throw new RuntimeException("unwrap")

  def isWrapperFor(iface: Class[_]) = false

  def setTypeMap(map: Map[String, Class[_]]) = {}

  def setTransactionIsolation(level: Int) = {}

  def setSavepoint(name: String) = null

  def setSavepoint = null

  def setReadOnly(readOnly: Boolean) = {}

  def setHoldability(holdability: Int) = {}

  def setClientInfo(properties: Properties) = {}

  def setClientInfo(name: String, value: String) = {}

  def setCatalog(catalog: String) = {}

  def setAutoCommit(autoCommit: Boolean) = {}

  def rollback(savepoint: Savepoint) = {}

  def rollback = {}

  def releaseSavepoint(savepoint: Savepoint) = {}

  def prepareStatement(sql: String, resultSetType: Int, resultSetConcurrency: Int, resultSetHoldability: Int) = null

  def prepareStatement(sql: String, resultSetType: Int, resultSetConcurrency: Int) = null

  def prepareStatement(sql: String, columnNames: scala.Array[String]) = null

  def prepareStatement(sql: String, columnIndexes: scala.Array[Int]) = null

  def prepareStatement(sql: String, autoGeneratedKeys: Int) = null

  def prepareStatement(sql: String) = null

  def prepareCall(sql: String, resultSetType: Int, resultSetConcurrency: Int, resultSetHoldability: Int) = null

  def prepareCall(sql: String, resultSetType: Int, resultSetConcurrency: Int) = null

  def prepareCall(sql: String) = null

  def nativeSQL(sql: String) = ""

  def isValid(timeout: Int) = false

  def isReadOnly = false

  def isClosed = false

  def getWarnings = null

  def getTypeMap = null

  def getTransactionIsolation = 0

  def getMetaData = new TestedDatabaseMetaData(this)

  def getHoldability = 0

  def getClientInfo(name: String) = ""

  def getClientInfo = null

  def getCatalog = ""

  def getAutoCommit = false

  def createStruct(typeName: String, attributes: scala.Array[AnyRef]) = null

  def createStatement(resultSetType: Int, resultSetConcurrency: Int, resultSetHoldability: Int) = new TestedStatement(this)

  def createStatement(resultSetType: Int, resultSetConcurrency: Int) = new TestedStatement(this)

  def createStatement = new TestedStatement(this)

  def createSQLXML = null

  def createNClob = null

  def createClob = null

  def createBlob = null

  def createArrayOf(typeName: String, elements: scala.Array[AnyRef]) = null

  def commit = {}

  def close = {}

  def clearWarnings = {}
}

class TestedStatement(val con : TestedConnection) extends java.sql.Statement {
  def unwrap[T](iface: Class[T]) = throw new RuntimeException("unwrap")

  def isWrapperFor(iface: Class[_]) = false

  def setQueryTimeout(seconds: Int) = {}

  def setPoolable(poolable: Boolean) = {}

  def setMaxRows(max: Int) = {}

  def setMaxFieldSize(max: Int) = {}

  def setFetchSize(rows: Int) = {}

  def setFetchDirection(direction: Int) = {}

  def setEscapeProcessing(enable: Boolean) = {}

  def setCursorName(name: String) = {}

  def isPoolable = false

  def isClosed = false

  def getWarnings = null

  def getUpdateCount = 0

  def getResultSetType = 0

  def getResultSetHoldability = 0

  def getResultSetConcurrency = 0

  def getResultSet = null

  def getQueryTimeout = 0

  def getMoreResults(current: Int) = false

  def getMoreResults = false

  def getMaxRows = 0

  def getMaxFieldSize = 0

  def getGeneratedKeys = null

  def getFetchSize = 0

  def getFetchDirection = 0

  def getConnection = null

  def executeUpdate(sql: String, columnNames: scala.Array[String]) = 0

  def executeUpdate(sql: String, columnIndexes: scala.Array[Int]) = 0

  def executeUpdate(sql: String, autoGeneratedKeys: Int) = 0

  def executeUpdate(sql: String) = 0

  def executeQuery(sql: String) = con.ds.results.find(_.isDefinedAt(sql) ) match {
    case Some(s) => s(sql)
    case _ => throw new RuntimeException("Could' not execute \"%s\"".format(sql))
  }

  def executeBatch = null

  def execute(sql: String, columnNames: scala.Array[String]) = false

  def execute(sql: String, columnIndexes: scala.Array[Int]) = false

  def execute(sql: String, autoGeneratedKeys: Int) = false

  def execute(sql: String) = false

  def close = {}

  def clearWarnings = {}

  def clearBatch = {}

  def cancel = {}

  def addBatch(sql: String) = {}
}

class TestedColumn(val name : String) {
  override def toString = name
}

class TestedResultSetMetaData(val columns : Seq[TestedColumn]) extends ResultSetMetaData {
  def indexOf(column : String) : Int = columns.indexWhere{_.name == column}

  def unwrap[T](iface: Class[T]) = throw new RuntimeException("unwrap")

  def isWrapperFor(iface: Class[_]) = false

  def isWritable(column: Int) = false

  def isSigned(column: Int) = false

  def isSearchable(column: Int) = false

  def isReadOnly(column: Int) = false

  def isNullable(column: Int) = 0

  def isDefinitelyWritable(column: Int) = false

  def isCurrency(column: Int) = false

  def isCaseSensitive(column: Int) = false

  def isAutoIncrement(column: Int) = false

  def getTableName(column: Int) = ""

  def getSchemaName(column: Int) = ""

  def getScale(column: Int) = 0

  def getPrecision(column: Int) = 0

  def getColumnTypeName(column: Int) = ""

  def getColumnType(column: Int) = 0

  def getColumnName(column: Int) = columns(column - 1).name

  def getColumnLabel(column: Int) = columns(column - 1).name

  def getColumnDisplaySize(column: Int) = 0

  def getColumnCount = columns.size

  def getColumnClassName(column: Int) = ""

  def getCatalogName(column: Int) = ""
}

class TestedResultSet(val meta : TestedResultSetMetaData, val rows : Seq[Seq[_]]) extends ResultSet {
  var row : Seq[_] = null
  val iterator : Iterator[Seq[_]] = rows.iterator

  def previous = false

  def next =
    if(iterator.hasNext){
      row = iterator.next
      true
    }
    else{
      false
    }

  def isLast = !iterator.hasNext

  def unwrap[T](iface: Class[T]) = throw new RuntimeException("unwrap")

  def isWrapperFor(iface: Class[_]) = false

  def wasNull = false

  def updateTimestamp(columnLabel: String, x: Timestamp) = {}

  def updateTimestamp(columnIndex: Int, x: Timestamp) = {}

  def updateTime(columnLabel: String, x: Time) = {}

  def updateTime(columnIndex: Int, x: Time) = {}

  def updateString(columnLabel: String, x: String) = {}

  def updateString(columnIndex: Int, x: String) = {}

  def updateSQLXML(columnLabel: String, xmlObject: SQLXML) = {}

  def updateSQLXML(columnIndex: Int, xmlObject: SQLXML) = {}

  def updateShort(columnLabel: String, x: Short) = {}

  def updateShort(columnIndex: Int, x: Short) = {}

  def updateRowId(columnLabel: String, x: RowId) = {}

  def updateRowId(columnIndex: Int, x: RowId) = {}

  def updateRow = {}

  def updateRef(columnLabel: String, x: java.sql.Ref) = {}

  def updateRef(columnIndex: Int, x: java.sql.Ref) = {}

  def updateObject(columnLabel: String, x: Any, scaleOrLength: Int) = {}

  def updateObject(columnLabel: String, x: Any) = {}

  def updateObject(columnIndex: Int, x: Any, scaleOrLength: Int) = {}

  def updateObject(columnIndex: Int, x: Any) = {}

  def updateNull(columnLabel: String) = {}

  def updateNull(columnIndex: Int) = {}

  def updateNString(columnLabel: String, nString: String) = {}

  def updateNString(columnIndex: Int, nString: String) = {}

  def updateNClob(columnLabel: String, reader: Reader, length: Long) = {}

  def updateNClob(columnLabel: String, reader: Reader) = {}

  def updateNClob(columnLabel: String, nClob: NClob) = {}

  def updateNClob(columnIndex: Int, reader: Reader, length: Long) = {}

  def updateNClob(columnIndex: Int, reader: Reader) = {}

  def updateNClob(columnIndex: Int, nClob: NClob) = {}

  def updateNCharacterStream(columnLabel: String, reader: Reader, length: Long) = {}

  def updateNCharacterStream(columnLabel: String, reader: Reader) = {}

  def updateNCharacterStream(columnIndex: Int, x: Reader, length: Long) = {}

  def updateNCharacterStream(columnIndex: Int, x: Reader) = {}

  def updateLong(columnLabel: String, x: Long) = {}

  def updateLong(columnIndex: Int, x: Long) = {}

  def updateInt(columnLabel: String, x: Int) = {}

  def updateInt(columnIndex: Int, x: Int) = {}

  def updateFloat(columnLabel: String, x: Float) = {}

  def updateFloat(columnIndex: Int, x: Float) = {}

  def updateDouble(columnLabel: String, x: Double) = {}

  def updateDouble(columnIndex: Int, x: Double) = {}

  def updateDate(columnLabel: String, x: Date) = {}

  def updateDate(columnIndex: Int, x: Date) = {}

  def updateClob(columnLabel: String, x: Clob) = {}

  def updateClob(columnLabel: String, reader: Reader, length: Long) = {}

  def updateClob(columnLabel: String, reader: Reader) = {}

  def updateClob(columnIndex: Int, x: Clob) = {}

  def updateClob(columnIndex: Int, reader: Reader, length: Long) = {}

  def updateClob(columnIndex: Int, reader: Reader) = {}

  def updateCharacterStream(columnLabel: String, reader: Reader, length: Long) = {}

  def updateCharacterStream(columnLabel: String, reader: Reader, length: Int) = {}

  def updateCharacterStream(columnLabel: String, reader: Reader) = {}

  def updateCharacterStream(columnIndex: Int, x: Reader, length: Long) = {}

  def updateCharacterStream(columnIndex: Int, x: Reader, length: Int) = {}

  def updateCharacterStream(columnIndex: Int, x: Reader) = {}

  def updateBytes(columnLabel: String, x: scala.Array[Byte]) = {}

  def updateBytes(columnIndex: Int, x: scala.Array[Byte]) = {}

  def updateByte(columnLabel: String, x: Byte) = {}

  def updateByte(columnIndex: Int, x: Byte) = {}

  def updateBoolean(columnLabel: String, x: Boolean) = {}

  def updateBoolean(columnIndex: Int, x: Boolean) = {}

  def updateBlob(columnLabel: String, x: Blob) = {}

  def updateBlob(columnLabel: String, inputStream: InputStream, length: Long) = {}

  def updateBlob(columnLabel: String, inputStream: InputStream) = {}

  def updateBlob(columnIndex: Int, x: Blob) = {}

  def updateBlob(columnIndex: Int, inputStream: InputStream, length: Long) = {}

  def updateBlob(columnIndex: Int, inputStream: InputStream) = {}

  def updateBinaryStream(columnLabel: String, x: InputStream, length: Long) = {}

  def updateBinaryStream(columnLabel: String, x: InputStream, length: Int) = {}

  def updateBinaryStream(columnLabel: String, x: InputStream) = {}

  def updateBinaryStream(columnIndex: Int, x: InputStream, length: Long) = {}

  def updateBinaryStream(columnIndex: Int, x: InputStream, length: Int) = {}

  def updateBinaryStream(columnIndex: Int, x: InputStream) = {}

  def updateBigDecimal(columnLabel: String, x: BigDecimal) = {}

  def updateBigDecimal(columnIndex: Int, x: BigDecimal) = {}

  def updateAsciiStream(columnLabel: String, x: InputStream, length: Long) = {}

  def updateAsciiStream(columnLabel: String, x: InputStream, length: Int) = {}

  def updateAsciiStream(columnLabel: String, x: InputStream) = {}

  def updateAsciiStream(columnIndex: Int, x: InputStream, length: Long) = {}

  def updateAsciiStream(columnIndex: Int, x: InputStream, length: Int) = {}

  def updateAsciiStream(columnIndex: Int, x: InputStream) = {}

  def updateArray(columnLabel: String, x: Array) = {}

  def updateArray(columnIndex: Int, x: Array) = {}

  def setFetchSize(rows: Int) = {}

  def setFetchDirection(direction: Int) = {}

  def rowUpdated = false

  def rowInserted = false

  def rowDeleted = false

  def relative(rows: Int) = false

  def refreshRow = {}



  def moveToInsertRow = {}

  def moveToCurrentRow = {}

  def last = false



  def isFirst = false

  def isClosed = false

  def isBeforeFirst = false

  def isAfterLast = false

  def insertRow = {}

  def getWarnings = null

  def getURL(columnLabel: String) = null

  def getURL(columnIndex: Int) = null

  def getUnicodeStream(columnLabel: String) = null

  def getUnicodeStream(columnIndex: Int) = null

  def getType = 0

  def getTimestamp(columnLabel: String, cal: Calendar) = null

  def getTimestamp(columnLabel: String) = null

  def getTimestamp(columnIndex: Int, cal: Calendar) = null

  def getTimestamp(columnIndex: Int) = null

  def getTime(columnLabel: String, cal: Calendar) = null

  def getTime(columnLabel: String) = null

  def getTime(columnIndex: Int, cal: Calendar) = null

  def getTime(columnIndex: Int) = null

  def getString(columnLabel: String) = row(meta.indexOf(columnLabel)).toString

  def getString(columnIndex: Int) = row.take(columnIndex).toString

  def getStatement = null

  def getSQLXML(columnLabel: String) = null

  def getSQLXML(columnIndex: Int) = null

  def getShort(columnLabel: String) = 0

  def getShort(columnIndex: Int) = 0

  def getRowId(columnLabel: String) = null

  def getRowId(columnIndex: Int) = null

  def getRow = 0

  def getRef(columnLabel: String) = null

  def getRef(columnIndex: Int) = null

  def getObject(columnLabel: String, map: Map[String, Class[_]]) = null

  def getObject(columnLabel: String) = {
    val i = meta.indexOf(columnLabel)
    if(i == -1) {
      throw new RuntimeException("Column \"%s\" not found.".format(columnLabel))
    }
    row(i).asInstanceOf[Object]
  }

  def getObject(columnIndex: Int, map: Map[String, Class[_]]) = null

  def getObject(columnIndex: Int) = row(columnIndex - 1).asInstanceOf[Object]

  def getNString(columnLabel: String) = ""

  def getNString(columnIndex: Int) = ""

  def getNClob(columnLabel: String) = null

  def getNClob(columnIndex: Int) = null

  def getNCharacterStream(columnLabel: String) = null

  def getNCharacterStream(columnIndex: Int) = null

  def getMetaData = meta

  def getLong(columnLabel: String) = 0L

  def getLong(columnIndex: Int) = 0L

  def getInt(columnLabel: String) = row(meta.indexOf(columnLabel)).asInstanceOf[Int]

  def getInt(columnIndex: Int) = 0

  def getHoldability = 0

  def getFloat(columnLabel: String) = 0

  def getFloat(columnIndex: Int) = 0

  def getFetchSize = 0

  def getFetchDirection = 0

  def getDouble(columnLabel: String) = 0.0

  def getDouble(columnIndex: Int) = 0.0

  def getDate(columnLabel: String, cal: Calendar) = null

  def getDate(columnLabel: String) = null

  def getDate(columnIndex: Int, cal: Calendar) = null

  def getDate(columnIndex: Int) = null

  def getCursorName = ""

  def getConcurrency = 0

  def getClob(columnLabel: String) = null

  def getClob(columnIndex: Int) = null

  def getCharacterStream(columnLabel: String) = null

  def getCharacterStream(columnIndex: Int) = null

  def getBytes(columnLabel: String) = null

  def getBytes(columnIndex: Int) = null

  def getByte(columnLabel: String) = 0

  def getByte(columnIndex: Int) = 0

  def getBoolean(columnLabel: String) = false

  def getBoolean(columnIndex: Int) = false

  def getBlob(columnLabel: String) = null

  def getBlob(columnIndex: Int) = null

  def getBinaryStream(columnLabel: String) = null

  def getBinaryStream(columnIndex: Int) = null

  def getBigDecimal(columnLabel: String, scale: Int) = null

  def getBigDecimal(columnLabel: String) = null

  def getBigDecimal(columnIndex: Int, scale: Int) = null

  def getBigDecimal(columnIndex: Int) = null

  def getAsciiStream(columnLabel: String) = null

  def getAsciiStream(columnIndex: Int) = null

  def getArray(columnLabel: String) = null

  def getArray(columnIndex: Int) = null

  def first = false

  def findColumn(columnLabel: String) = 0

  def deleteRow = {}

  def close = {}

  def clearWarnings = {}

  def cancelRowUpdates = {}

  def beforeFirst = {}

  def afterLast = {}

  def absolute(row: Int) = false
}

class TestedDatabaseMetaData(val con : TestedConnection) extends DatabaseMetaData {
  def unwrap[T](iface: Class[T]) = throw new RuntimeException("unwrap")

  def isWrapperFor(iface: Class[_]) = false

  def usesLocalFiles = false

  def usesLocalFilePerTable = false

  def updatesAreDetected(`type` : Int) = false

  def supportsUnionAll = false

  def supportsUnion = false

  def supportsTransactions = false

  def supportsTransactionIsolationLevel(level: Int) = false

  def supportsTableCorrelationNames = false

  def supportsSubqueriesInQuantifieds = false

  def supportsSubqueriesInIns = false

  def supportsSubqueriesInExists = false

  def supportsSubqueriesInComparisons = false

  def supportsStoredProcedures = false

  def supportsStoredFunctionsUsingCallSyntax = false

  def supportsStatementPooling = false

  def supportsSelectForUpdate = false

  def supportsSchemasInTableDefinitions = false

  def supportsSchemasInProcedureCalls = false

  def supportsSchemasInPrivilegeDefinitions = false

  def supportsSchemasInIndexDefinitions = false

  def supportsSchemasInDataManipulation = false

  def supportsSavepoints = false

  def supportsResultSetType(`type` : Int) = false

  def supportsResultSetHoldability(holdability: Int) = false

  def supportsResultSetConcurrency(`type` : Int, concurrency: Int) = false

  def supportsPositionedUpdate = false

  def supportsPositionedDelete = false

  def supportsOuterJoins = false

  def supportsOrderByUnrelated = false

  def supportsOpenStatementsAcrossRollback = false

  def supportsOpenStatementsAcrossCommit = false

  def supportsOpenCursorsAcrossRollback = false

  def supportsOpenCursorsAcrossCommit = false

  def supportsNonNullableColumns = false

  def supportsNamedParameters = false

  def supportsMultipleTransactions = false

  def supportsMultipleResultSets = false

  def supportsMultipleOpenResults = false

  def supportsMixedCaseQuotedIdentifiers = false

  def supportsMixedCaseIdentifiers = false

  def supportsMinimumSQLGrammar = false

  def supportsLimitedOuterJoins = false

  def supportsLikeEscapeClause = false

  def supportsIntegrityEnhancementFacility = false

  def supportsGroupByUnrelated = false

  def supportsGroupByBeyondSelect = false

  def supportsGroupBy = false

  def supportsGetGeneratedKeys = false

  def supportsFullOuterJoins = false

  def supportsExtendedSQLGrammar = false

  def supportsExpressionsInOrderBy = false

  def supportsDifferentTableCorrelationNames = false

  def supportsDataManipulationTransactionsOnly = false

  def supportsDataDefinitionAndDataManipulationTransactions = false

  def supportsCorrelatedSubqueries = false

  def supportsCoreSQLGrammar = false

  def supportsConvert(fromType: Int, toType: Int) = false

  def supportsConvert = false

  def supportsColumnAliasing = false

  def supportsCatalogsInTableDefinitions = false

  def supportsCatalogsInProcedureCalls = false

  def supportsCatalogsInPrivilegeDefinitions = false

  def supportsCatalogsInIndexDefinitions = false

  def supportsCatalogsInDataManipulation = false

  def supportsBatchUpdates = false

  def supportsANSI92IntermediateSQL = false

  def supportsANSI92FullSQL = false

  def supportsANSI92EntryLevelSQL = false

  def supportsAlterTableWithDropColumn = false

  def supportsAlterTableWithAddColumn = false

  def storesUpperCaseQuotedIdentifiers = false

  def storesUpperCaseIdentifiers = false

  def storesMixedCaseQuotedIdentifiers = false

  def storesMixedCaseIdentifiers = false

  def storesLowerCaseQuotedIdentifiers = false

  def storesLowerCaseIdentifiers = false

  def ownUpdatesAreVisible(`type` : Int) = false

  def ownInsertsAreVisible(`type` : Int) = false

  def ownDeletesAreVisible(`type` : Int) = false

  def othersUpdatesAreVisible(`type` : Int) = false

  def othersInsertsAreVisible(`type` : Int) = false

  def othersDeletesAreVisible(`type` : Int) = false

  def nullsAreSortedLow = false

  def nullsAreSortedHigh = false

  def nullsAreSortedAtStart = false

  def nullsAreSortedAtEnd = false

  def nullPlusNonNullIsNull = false

  def locatorsUpdateCopy = false

  def isReadOnly = false

  def isCatalogAtStart = false

  def insertsAreDetected(`type` : Int) = false

  def getVersionColumns(catalog: String, schema: String, table: String) = null

  def getUserName = ""

  def getURL = ""

  def getUDTs(catalog: String, schemaPattern: String, typeNamePattern: String, types: scala.Array[Int]) = null

  def getTypeInfo = null

  def getTimeDateFunctions = ""

  def getTableTypes = null

  def getTables(catalog: String, schemaPattern: String, tableNamePattern: String, types: scala.Array[String]) = null

  def getTablePrivileges(catalog: String, schemaPattern: String, tableNamePattern: String) = null

  def getSystemFunctions = ""

  def getSuperTypes(catalog: String, schemaPattern: String, typeNamePattern: String) = null

  def getSuperTables(catalog: String, schemaPattern: String, tableNamePattern: String) = null

  def getStringFunctions = ""

  def getSQLStateType = 0

  def getSQLKeywords = ""

  def getSearchStringEscape = ""

  def getSchemaTerm = ""

  def getSchemas(catalog: String, schemaPattern: String) = null

  def getSchemas = null

  def getRowIdLifetime = null

  def getResultSetHoldability = 0

  def getProcedureTerm = ""

  def getProcedures(catalog: String, schemaPattern: String, procedureNamePattern: String) = null

  def getProcedureColumns(catalog: String, schemaPattern: String, procedureNamePattern: String, columnNamePattern: String) = null

  def getPrimaryKeys(catalog: String, schema: String, table: String) = null

  def getNumericFunctions = ""

  def getMaxUserNameLength = 0

  def getMaxTablesInSelect = 0

  def getMaxTableNameLength = 0

  def getMaxStatements = 0

  def getMaxStatementLength = 0

  def getMaxSchemaNameLength = 0

  def getMaxRowSize = 0

  def getMaxProcedureNameLength = 0

  def getMaxIndexLength = 0

  def getMaxCursorNameLength = 0

  def getMaxConnections = 0

  def getMaxColumnsInTable = 0

  def getMaxColumnsInSelect = 0

  def getMaxColumnsInOrderBy = 0

  def getMaxColumnsInIndex = 0

  def getMaxColumnsInGroupBy = 0

  def getMaxColumnNameLength = 0

  def getMaxCharLiteralLength = 0

  def getMaxCatalogNameLength = 0

  def getMaxBinaryLiteralLength = 0

  def getJDBCMinorVersion = 0

  def getJDBCMajorVersion = 0

  def getIndexInfo(catalog: String, schema: String, table: String, unique: Boolean, approximate: Boolean) = null

  def getImportedKeys(catalog: String, schema: String, table: String) = null

  def getIdentifierQuoteString = ""

  def getFunctions(catalog: String, schemaPattern: String, functionNamePattern: String) = null

  def getFunctionColumns(catalog: String, schemaPattern: String, functionNamePattern: String, columnNamePattern: String) = null

  def getExtraNameCharacters = ""

  def getExportedKeys(catalog: String, schema: String, table: String) = null

  def getDriverVersion = ""

  def getDriverName = ""

  def getDriverMinorVersion = 0

  def getDriverMajorVersion = 0

  def getDefaultTransactionIsolation = 0

  def getDatabaseProductVersion = ""

  def getDatabaseProductName = ""

  def getDatabaseMinorVersion = 0

  def getDatabaseMajorVersion = 0

  def getCrossReference(parentCatalog: String, parentSchema: String, parentTable: String, foreignCatalog: String, foreignSchema: String, foreignTable: String) = null

  def getConnection = null

  def getColumns(catalog: String, schemaPattern: String, tableNamePattern: String, columnNamePattern: String) = null

  def getColumnPrivileges(catalog: String, schema: String, table: String, columnNamePattern: String) = null

  def getClientInfoProperties = null

  def getCatalogTerm = ""

  def getCatalogSeparator = ""

  def getCatalogs = null

  def getBestRowIdentifier(catalog: String, schema: String, table: String, scope: Int, nullable: Boolean) = null

  def getAttributes(catalog: String, schemaPattern: String, typeNamePattern: String, attributeNamePattern: String) = null

  def doesMaxRowSizeIncludeBlobs = false

  def deletesAreDetected(`type` : Int) = false

  def dataDefinitionIgnoredInTransactions = false

  def dataDefinitionCausesTransactionCommit = false

  def autoCommitFailureClosesAllResultSets = false

  def allTablesAreSelectable = false

  def allProceduresAreCallable = false


}