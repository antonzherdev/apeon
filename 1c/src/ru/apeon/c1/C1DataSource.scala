package ru.apeon.c1

import xml.NodeSeq
import ru.apeon.core.entity._
import akka.util.Logging
import com.ipc.oce.objects._
import com.ipc.oce.{OCVariant}

class C1DataSource(val dataSource : DataSource) extends DataSourceImpl with Logging{
  def persistentStore(xml: NodeSeq) = new C1PersistentStore(dataSource.name, xml.\("@url").text,
    xml.\("@userName").text, xml.\("@password").text)

  def toVariant(e: Entity, field: FieldWithSource): OCVariant = e.data(field.name) match{
    case e : Entity => new OCVariant(e.id.data.head.asInstanceOf[C1Ref].ref)
    case r => new OCVariant(r)
  }

  def tabularName(e: Description): String = {
    e.table.name.split('.').last
  }

  def writeAttributes(doc: AttributeBean, e: Entity, fields: collection.Set[FieldWithSource]) {
    fields.foreach {
      field => {
        doc.setAttributeValue(field.columnName(dataSource), toVariant(e, field))
      }
    }
  }

  def writeTabAttributes(tab: OCTabularSectionManager, number: Int, e: Entity, fields: collection.Set[FieldWithSource]) {
    val row = tab.get(number - 1)
    fields.filterNot{_.isPrimaryKey}.foreach {
      field => {
        row.setColumnValue(field.columnName(dataSource), toVariant(e, field))
      }
    }
  }

  override def update(em: EntityManager, e: Entity, fields: collection.Set[FieldWithSource]) {
    def tabular: String = tabularName(e.id.description)

    e.id.data match {
      case Seq(id : C1Ref) => {
        log.debug("Updating 1c document %s", id.uuid)
        id.ref.getObject match {
          case doc : OCDocumentObject => {
            writeAttributes(doc, e, fields)
            doc.write()
          }
          case doc : OCCatalogObject => {
            writeAttributes(doc, e, fields)
            doc.write()
          }
        }
      }
      case Seq(id : C1Ref, number : Int) => {
        log.debug("Updating 1c document tabular %s in row %d", id.uuid, number)
        id.ref.getObject match {
          case doc : OCDocumentObject => {
            writeTabAttributes(doc.getTabularSection(tabular), number, e, fields)
            doc.write()
          }
          case doc : OCCatalogObject => {
            writeTabAttributes(doc.getTabularSection(tabular), number, e, fields)
            doc.write()
          }
        }
      }
    }
  }

  private def fromVariant(field: Field, v: OCVariant): Any = {
    val m = field.asInstanceOf[Attribute].dataType match {
      case AttributeDataTypeBoolean() => v.getBoolean
      case c: AttributeDataTypeString => v.getString
      case d: AttributeDataTypeDate => v.getDate
      case d: AttributeDataTypeDecimal => BigDecimal(
        try {
          v.getDouble.doubleValue
        }
        catch {
          case e: Throwable => v.getInt.doubleValue
        }, BigDecimal.defaultMathContext)
      case d: AttributeDataTypeInteger => v.getInt
    }
    m
  }

  private def tabular(obj: _OCCommonObject, e: Description): OCTabularSectionManager = {
    val tabName = tabularName(e)
    obj match {
      case doc: OCDocumentObject => doc.getTabularSection(tabName)
      case doc: OCCatalogObject => doc.getTabularSection(tabName)
    }
  }

  override def lazyLoad(em: EntityManager, entity: Entity, many: ToMany) : Set[Entity] = {
    val ref: _OCCommonRef = entity.id.data.head.asInstanceOf[C1Ref].ref
    val uuid = ref.getUUID.toString
    val tab = tabular(ref.getObject, many.entity)
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
          b += ((field.name, fromVariant(field, v)))
        }
      }

      many.entity.fields.filter{
        field => field.isInstanceOf[ToOne] && field.name != "parent"
      }.map{
        field => {
          val ref = row.getColumnValue(field.asInstanceOf[ToOne].columnName(dataSource)).value[_OCCommonRef]
          b += ((field.name, C1Ref(ref.getUUID.toString, ref)))
        }
      }

      em.toEntity(dataSource, many.entity, b.result())
    }.toSet
  }

  override def lazyLoad(em: EntityManager, entity: Entity, one: ToOne, data: Any) = data match {
    case r : C1Ref => {
      val b = collection.mutable.Map.newBuilder[String, Any]
      b += (("id", r))
      val o = r.ref.getObject.asInstanceOf[AttributeBean]

      one.entity.attributes.filterNot(_.isPrimaryKey).foreach{field =>
        val v = o.getAttributeValue(field.columnName(dataSource))
        b += ((field.name, fromVariant(field, v)))
      }
      one.entity.fields.filter{_.isInstanceOf[ToOne]}.foreach{field =>
        val ref = o.getAttributeValue(field.asInstanceOf[ToOne].columnName(dataSource)).value[_OCCommonRef]
        b += ((field.name, C1Ref(ref.getUUID.toString, ref)))
      }
      em.toEntity(dataSource, one.entity, b.result())
    }
    case _ => throw new RuntimeException("Not ref")
  }

  override def delete(em: EntityManager, e: Entity) {
    e.id.data match {
      case Seq(r : C1Ref) => r.ref.getObject match {
        case doc : OCDocumentObject => {
          doc.setDeletionMark(true)
          doc.write()
        }
        case doc : OCCatalogObject => {
          doc.setDeletionMark(true)
          doc.write()
        }
      }
      case Seq(r : C1Ref, number : Int) => {
        val o = r.ref.getObject
        tabular(o, e.id.description).delete(number - 1)
        o.write()
      }
    }
  }

  override def insert(em: EntityManager, e: Entity) {
    val app = e.id.store.asInstanceOf[C1PersistentStore].app
    val d = e.id.description

    val fields = toInsertColumns(em, e).map{
      col => e.id.description.field(col.columnName).asInstanceOf[FieldWithSource]
    }.toSet

    d.table.name.split('.').toSeq match {
      case Seq(name) => {
        d.table.schema match {
          case "Document" => {
            val doc = app.getDocumentManager(name).createDocument
            writeAttributes(doc, e, fields)
            doc.write()
            val ref = doc.getRef
            e.saveId(C1Ref(ref.getUUID.toString, ref))
          }
          case "Catalog" => {
            val doc = app.getCatalogManager(name).createItem
            writeAttributes(doc, e, fields)
            doc.write()
            val ref = doc.getRef
            e.saveId(C1Ref(ref.getUUID.toString, ref))
          }
        }
      }
      case Seq(name, tabular) => {
        val dc = e("parent").asInstanceOf[Entity].id.data.head.asInstanceOf[C1Ref].ref.getObject
        d.table.schema match {
          case "Document" => {
            val doc = dc.asInstanceOf[OCDocumentObject]
            val tab = doc.getTabularSection(tabular)
            val row = tab.add()
            writeTabAttributes(tab, row.getLineNumber.intValue, e, fields)
            doc.write()
            val ref = doc.getRef
            e.saveId(Seq(C1Ref(ref.getUUID.toString, ref), row.getLineNumber.intValue))
          }
          case "Catalog" => {
            val doc = dc.asInstanceOf[OCCatalogObject]
            val tab = doc.getTabularSection(tabular)
            val row = tab.add()
            writeTabAttributes(tab, row.getLineNumber.intValue, e, fields)
            doc.write()
            val ref = doc.getRef
            e.saveId(Seq(C1Ref(ref.getUUID.toString, ref), row.getLineNumber.intValue))
          }
        }
      }
    }
  }
}