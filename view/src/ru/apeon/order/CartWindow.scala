package ru.apeon.order

import ru.apeon.cloud.Cloud
import com.vaadin.data.Property.{ValueChangeEvent, ValueChangeListener}
import ru.apeon.vaadin._
import com.vaadin.ui._

/**
 * @author Anton Zherdev
 */

class CartWindow extends MyWindow("Корзина") {
  setWidth("60%")
  setHeight("50%")
  center()

  val data : MyIndexedContainer = new MyIndexedContainer
  data.addContainerProperty("id", classOf[Integer], 0)
  data.addContainerProperty("name", classOf[String], "")
  data.addContainerProperty("kol", classOf[KolTextField], 0.0)
  data.addContainerProperty("price", classOf[BigDecimal], 0.0)
  data.addContainerProperty("sum", classOf[BigDecimal], 0.0)
  data.addContainerProperty("delete", classOf[Button], null)
  Cloud.query("MyCart") foreach { m =>
    var id: Object = data.addItem
    data.update(id, "id", m("id"))
    data.update(id, "name", m./("inventory")("name"))

    var kol1 : BigDecimal = m ! "kol1"
    kol1 = kol1.setScale(2)
    var price : BigDecimal = m./("inventory").!("price")
    price = price.setScale(2)
    data.update(id, "kol", new KolTextField(id, kol1))
    data.update(id, "price", price)
    data.update(id, "sum", (kol1 * price).setScale(2))
    data.update(id, "delete", new DeleteCartButton(id))
  }


  val table: Table = new Table
  table.setContainerDataSource(data)
  table.setColumnHeader("name", "ТМЦ")
  table.setColumnHeader("kol", "Количество")
  table.setColumnHeader("price", "Цена")
  table.setColumnHeader("sum", "Сумма")
  table.setColumnHeader("delete", "")
  table.setVisibleColumns(Array("name", "kol", "price", "sum", "delete"))

  val bid : MyButton = MyButton("Оформить заказ") {e : MyClickEvent =>
    val par : Window = this.getParent.asInstanceOf[Window]

    val wnd = new BidWindow
    par.removeWindow(this)
    par.addWindow(wnd)
  }

  val total = new Label("Итого")
  updateTotal

  def updateTotal = {
    total.setValue("Итого: " +
            data.foldLeft(BigDecimal(0).setScale(2)){ (sum, row) =>
              sum + row("sum")
            }.toString + " руб.")
  }

  setFullContent(vertical addExpanded {
    table
  } add {
    horizontal.margin.spacing.add{
      bid
    }.add {
      total
    }.align(Right, Center)
  })

  class DeleteCartButton(val row : Object) extends MyButton {
    setCaption("Удалить")
    addListener{ e : MyClickEvent =>
      val id : Int = data(row, "id")
      val name : String = data(row, "name")
      Cloud.delete("Cart", id)
      data.removeItem(row)
      showNotification(name + " удалена из корзины")
    }
  }

  class KolTextField(val row : Object, var kol : BigDecimal) extends TextField {
    setImmediate(true)
    setValue(kol)
    this.addListener(new ValueChangeListener{
      def valueChange(event: ValueChangeEvent) = {
        val id : Int = data(row, "id")
        kol = BigDecimal(event.getProperty.getValue.toString)
        Cloud.update("Cart", id, Map("kol1" -> kol))
        data.update(row, "sum", kol * data[BigDecimal](row, "price"))
        updateTotal
        showNotification(event.getProperty.getValue.toString)
      }
    })
  }
}