package ru.apeon.c1

import xml.NodeSeq
import collection.Set
import com.ipc.oce.objects.OCDocumentObject
import ru.apeon.core.entity._
import com.ipc.oce.OCVariant

class C1DataSource(val dataSource : DataSource) extends DataSourceImpl{
  def persistentStore(xml: NodeSeq) = new C1PersistentStore(dataSource.name, xml.\("@url").text,
    xml.\("@userName").text, xml.\("@password").text)

  def toVariant(e: Entity, field: FieldWithSource): OCVariant = e.data(field.name) match{
    case e : Entity => new OCVariant(e.id.data.head)
    case r => new OCVariant(r)
  }

  override def update(em: EntityManager, e: Entity, fields: collection.Set[FieldWithSource]) {
    e.id.data match {
      case Seq(id : C1Ref) => {
        id.ref.getObject match {
          case doc : OCDocumentObject =>
            fields.foreach{
              field => doc.setAttributeValue(field.columnName(dataSource), toVariant(e, field))
            }
        }
      }
    }
  }
}