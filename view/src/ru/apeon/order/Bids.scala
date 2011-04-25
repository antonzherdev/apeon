package ru.apeon.order

import java.util.Date
import java.text.{SimpleDateFormat, DateFormat}
import com.vaadin.data.Property.{ValueChangeEvent, ValueChangeListener}
import java.io.Serializable
import com.vaadin.ui._
import ru.apeon.vaadin._
import ru.apeon.cloud.{DataMap, Cloud}

/**
 * @author Anton Zherdev
 */

class Bids extends MyApplication with Vaadin {
  def init = {
    setMainWindow(new BidsWindow)
  }
}

class BidsWindow extends MyWindow("Мои заявки") {
  val table: MyTable = new MyTable
  table.setColumnHeader("date", "Дата")
  table.setColumnHeader("nu", "Номер")
  table.setColumnHeader("organization", "Огранизация")
  table.setColumnHeader("state", "Состояние")
  table.setColumnHeader("sum", "Сумма")
  table.setSelectable(true)
  table.setImmediate(true)
  table.setSizeFull
  table.addListener(new ValueChangeListener {
    def valueChange(event: ValueChangeEvent) = {
      tableSelected
    }
  })

  val invTable : Table = new Table
  invTable.setWidth("100%")

  val showClosed : MyCheckBox = MyCheckBox("Показывать завершенные заказы") { e :MyCheckBoxEvent =>
    data = loadData(if(e.value) List("0", "1") else List("0"))
    updateData
  }

  val title : Label = new Label("")
  title.addStyleName("h1")

  val address : TextField = new TextField("Адрес доставки:")
  address.setReadOnly(true)
  address.setWidth("95%")
  val comment : TextField = new TextField("Комментарии:")
  comment.setRows(5)
  comment.setWidth("95%")
  comment.setReadOnly(true)

  setContent(
    toolbar {
      horizontal.add{
        showClosed
      }
    } body {
      horizontalSplit {
        table
      } split {
        vertical.add{
          vertical.margin.add{
            title
          }.add {
            form.add{
              address
            }.add{
              comment
            }
          }
        }.addExpanded{
          invTable
        }
      }
    }
    )

  var data : MyIndexedContainer = loadData(List("0"))
  updateData


  def loadData(isClose : List[String]) = {
    val ret = new MyIndexedContainer

    ret.addContainerProperty("id", classOf[java.lang.Integer], "")
    ret.addContainerProperty("date", classOf[FormatedDate], "")
    ret.addContainerProperty("nu", classOf[String], "")
    ret.addContainerProperty("state", classOf[String], "")
    ret.addContainerProperty("sum", classOf[BigDecimal], "")
    ret.addContainerProperty("rem", classOf[String], "")
    ret.addContainerProperty("organization", classOf[String], "")
    ret.addContainerProperty("address", classOf[String], "")
    Cloud.query("MyBids", Map("isClose" -> isClose)) foreach { m : DataMap =>
      val id: Object = ret.addItem
      ret.update(id, "id", m("id"))
      ret.update(id, "date", new FormatedDate(m("date").asInstanceOf[Date]))
      ret.update(id, "nu", m("nu"))
      ret.update(id, "state", m./("state")("name"))
      ret.update(id, "sum", m.dec("sum", 2))
      ret.update(id, "rem", m("rem"))
      ret.update(id, "organization", m./("organization")("name"))
      ret.update(id, "address", m./("address")("address"))
    }
    ret
  }

  def updateData = {
    table.setContainerDataSource(data)
    table.setVisibleColumns(Array("date", "nu", "organization", "state", "sum"))
  }



  def tableSelected = {
    val invData : MyIndexedContainer = new MyIndexedContainer
    invData.addContainerProperty("id", classOf[Integer], "")
    invData.addContainerProperty("inventory", classOf[String], "")
    invData.addContainerProperty("kol1", classOf[BigDecimal], "")
    invData.addContainerProperty("price", classOf[BigDecimal], "")
    invData.addContainerProperty("sum", classOf[BigDecimal], "")

    if(table.getValue != null) {
      title.setValue("№" + table("nu") + " от " + table("date"))

      comment.setReadOnly(false)
      comment.setValue(table("rem"))
      comment.setReadOnly(true)

      address.setReadOnly(false)
      address.setValue(table("address"))
      address.setReadOnly(true)

      val bid = Cloud.find("Bid", data.getId(table.getValue), List("id", "invs"))

      (bid * "invs").foreach{ m =>
        val id: Object = invData.addItem
        invData.update(id, "id", m("id"))
        invData.update(id, "inventory", m./("inventory")("name"))
        val kol = m.dec("kol1", 2)
        val sum = m.dec("sum", 2)
        invData.update(id, "kol1", kol)
        invData.update(id, "price", (sum/kol).setScale(2))
        invData.update(id, "sum", sum)
      }
    } else {
      comment.setReadOnly(false)
      comment.setValue("")
      comment.setReadOnly(true)

      address.setReadOnly(false)
      address.setValue("")
      address.setReadOnly(true)
    }
    invTable.setContainerDataSource(invData)
    invTable.setVisibleColumns(Array("inventory", "kol1", "price", "sum"))
    invTable.setColumnHeader("inventory", "Номенклатура")
    invTable.setColumnHeader("kol1", "Количество")
    invTable.setColumnHeader("price", "Цена")
    invTable.setColumnHeader("sum", "Сумма")
  }

  class FormatedDate(val date : Date) extends Serializable {
    val formater : DateFormat = new SimpleDateFormat("dd.MM.yyyy")

    override def toString = formater.format(date)
  }
}