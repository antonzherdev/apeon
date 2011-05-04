package ru.apeon.c1

import ru.apeon.core.entity.{DataSource, DataSourceImpl}
import xml.NodeSeq

class C1DataSource extends DataSourceImpl{
  def persistentStore(dataSource: DataSource, xml: NodeSeq) = new C1PersistentStore(dataSource.name, xml.\("@url").text,
    xml.\("@userName").text, xml.\("@password").text)
}