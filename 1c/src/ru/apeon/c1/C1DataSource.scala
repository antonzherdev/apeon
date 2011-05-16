package ru.apeon.c1

import xml.NodeSeq
import ru.apeon.core.entity.{Entity, EntityManager, DataSource, DataSourceImpl}
import collection.Set
import com.ipc.oce.objects.OCDocumentObject

class C1DataSource(val dataSource : DataSource) extends DataSourceImpl{
  def persistentStore(xml: NodeSeq) = new C1PersistentStore(dataSource.name, xml.\("@url").text,
    xml.\("@userName").text, xml.\("@password").text)

  override def update(em: EntityManager, e: Entity, columns: Set[String]) {
    e.id.data match {
      case Seq(id : C1Ref) => {
        id.ref.getObject match {
          case doc : OCDocumentObject =>
            columns.foreach{
              column => doc.setAttributeValue(e.id.description.field(column).asInstanceOf[FiledWith])
            }
        }
      }
    }
  }
}