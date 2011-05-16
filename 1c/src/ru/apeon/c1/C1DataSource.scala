package ru.apeon.c1

import xml.NodeSeq
import ru.apeon.core.entity.{DataSource, DataSourceImpl}

class C1DataSource(val dataSource : DataSource) extends DataSourceImpl{
  def persistentStore(xml: NodeSeq) = new C1PersistentStore(dataSource.name, xml.\("@url").text,
    xml.\("@userName").text, xml.\("@password").text)
}