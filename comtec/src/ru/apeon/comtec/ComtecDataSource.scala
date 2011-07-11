package ru.apeon.comtec

import xml.NodeSeq
import ru.apeon.core.entity.{SqlPersistentStore, DataSource, DataSourceImplLookup}
import ru.apeon.core._
import sql.Column


class ComtecDataSource(dataSource : DataSource) extends DataSourceImplLookup(dataSource) {
  override def persistentStore(xml: NodeSeq) =
    new SqlPersistentStore(dataSource.fullName,
      new sql.DataSource(lookup(dataSource, xml),new ComtecAsaSqlDialect),
      new ComtecSqlGenerator
    ) {
      override protected val _eql = new Eql {
        override protected def sqlRow(select: eql.Select, sqlSelect: sql.Select, rows: sql.Rows) =
          new sql.RowSyntax(rows.rs, sqlSelect) {
            override def value(column: Column, j: Int) = rs.getObject(j + 1) match {
              case d: java.math.BigDecimal => BigDecimal(d)

              //Это проблема ASA(Глюк #27). Если строка пустая, то она возращает Null. Нужно, чтобы это только для нее и выполнялось.
              case null => if (rs.getMetaData.getColumnClassName(j + 1) == "java.lang.String") "" else null

              case d => d
            }
          }
      }

//      override def getConnection = {
//        val con = super.getConnection
//        val stm = con.createStatement()
//        stm.execute("SET TEMPORARY OPTION CONNECTION_AUTHENTICATION='Company=Comtec Ltd;Application=Comtec for Business;Signature=000fa55157edb8e14d818eb4fe3db41447146f1571g50d0fe1fd35884b6336b6950a87abd70376da7e6'")
//        stm.close()
//        con
//      }
    }
}