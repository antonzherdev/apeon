package ru.apeon.comtec

import xml.NodeSeq
import ru.apeon.core.entity.{SqlPersistentStore, DataSource, DataSourceImplLookup}
import ru.apeon.core._
import sql.Column


class ComtecDataSource extends DataSourceImplLookup {
  override def persistentStore(dataSource: DataSource, xml: NodeSeq) =
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
    }
}