package ru.apeon.c1

import xml.NodeSeq
import ru.apeon.core.entity._
import com.ipc.oce.OCVariant
import akka.util.Logging
import com.ipc.oce.objects._

class C1DataSource(val dataSource : DataSource) extends DataSourceImpl with Logging{
  def persistentStore(xml: NodeSeq) = new C1PersistentStore(dataSource.name, xml.\("@url").text,
    xml.\("@userName").text, xml.\("@password").text)

  def toVariant(e: Entity, field: FieldWithSource): OCVariant = e.data(field.name) match{
    case e : Entity => new OCVariant(e.id.data.head)
    case r => new OCVariant(r)
  }

  def tabularName(e: Description): String = {
    e.table.name.split('.').last
  }

  override def update(em: EntityManager, e: Entity, fields: collection.Set[FieldWithSource]) {
    def writeAttributes(doc: AttributeBean) {
      fields.foreach {
        field => doc.setAttributeValue(field.columnName(dataSource), toVariant(e, field))
      }
    }

    def tabular: String = tabularName(e.id.description)

    def writeTabAttributes(tab: OCTabularSectionManager, number: Int) {
      val row = tab.get(number - 1)
      fields.foreach {
        field => row.setColumnValue(field.columnName(dataSource), toVariant(e, field))
      }
    }
    e.id.data match {
      case Seq(id : C1Ref) => {
        log.debug("Updating 1c document %s", id.uuid)
        id.ref.getObject match {
          case doc : OCDocumentObject => {
            writeAttributes(doc)
            doc.write()
          }
          case doc : OCCatalogObject => {
            writeAttributes(doc)
            doc.write()
          }
        }
      }
      case Seq(id : C1Ref, number : Int) => {
        log.debug("Updating 1c document tabular %s in row %d", id.uuid, number)
        id.ref.getObject match {
          case doc : OCDocumentObject => {
            writeTabAttributes(doc.getTabularSection(tabular), number)
            doc.write()
          }
          case doc : OCCatalogObject => {
            writeTabAttributes(doc.getTabularSection(tabular), number)
            doc.write()
          }
        }
      }
    }
  }

  override def lazyLoad(em: EntityManager, entity: Entity, many: ToMany) : Set[Entity] = {
    val tabName = tabularName(many.entity)
    val ref: _OCCommonRef = entity.id.data.head.asInstanceOf[C1Ref].ref
    val uuid = ref.getUUID.toString
    val tab = ref.getObject match {
      case doc : OCDocumentObject => doc.getTabularSection(tabName)
      case doc : OCCatalogObject => doc.getTabularSection(tabName)
    }
    Range(0, tab.size.intValue).map{i =>
      val row = tab.get(i)
      val b = collection.mutable.Map.newBuilder[String, Any]
      b += (("parent", C1Ref(uuid, ref)))
      b += (("number", i + 1))
      many.entity.fields.filter{
        field => field.isInstanceOf[Attribute] && field.name != "number"
      }.foreach{
        field => {
          val v = row.getColumnValue(field.asInstanceOf[Attribute].columnName(dataSource))
          val m = field.asInstanceOf[Attribute].dataType match {
            case AttributeDataTypeBoolean() => v.getBoolean
            case c : AttributeDataTypeChar => v.getString
            case d : AttributeDataTypeDate => v.getDate
            case d : AttributeDataTypeDateTime => v.getDate
            case d : AttributeDataTypeDecimal => BigDecimal(
              try{v.getDouble.doubleValue}
              catch {case e : Throwable => v.getInt.doubleValue}, BigDecimal.defaultMathContext)
            case d : AttributeDataTypeInteger => v.getInt
            case d : AttributeDataTypeText => v.getString
            case d : AttributeDataTypeTime => v.getDate
            case d : AttributeDataTypeVarchar => v.getString
          }
          b += ((field.name, m))
        }
      }
      em.toEntity(dataSource, many.entity, b.result())
    }.toSet
  }
}