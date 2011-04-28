package ru.apeon.comtec

import xml.NodeSeq
import ru.apeon.core.entity.{SqlPersistentStore, DataSource, DataSourceImplLookup}
import ru.apeon.core.sql

class ComtecDataSource extends DataSourceImplLookup {
  override def persistentStore(dataSource: DataSource, xml: NodeSeq) =
    new SqlPersistentStore(dataSource.fullName,
      new sql.DataSource(lookup(dataSource, xml),new ComtecAsaSqlDialect),
      new ComtecSqlGenerator
    )
}