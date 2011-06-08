package ru.apeon.order

import com.vaadin.ui.Window.Notification
import ru.apeon.vaadin.{MyIndexedContainer, MyClickEvent, MyButton, MyWindow}
import com.vaadin.ui.{AbstractSelect, ComboBox, Window, TextField}
import com.vaadin.data.Property.{ValueChangeEvent, ValueChangeListener}
import java.lang.String
import com.vaadin.ui.AbstractSelect.{NewItemHandler, Filtering}
import ru.apeon.cloud.{Cloud, DataMap}

/**
 * @author Anton Zherdev
 */

class BidWindow extends MyWindow("Оформление заказа") {
  setWidth("60%")
  setHeight("50%")
  center()

  val organization : ComboBox = new ComboBox("Организация:")
  organization.setWidth("100%")
  organization.setImmediate(true)
  organization.setItemCaptionMode(AbstractSelect.ITEM_CAPTION_MODE_PROPERTY);
  organization.setFilteringMode(Filtering.FILTERINGMODE_CONTAINS);
  organization.setNullSelectionAllowed(false);

  val address : ComboBox = new ComboBox("Адрес доставки:")
  address.setWidth("100%")
  address.setFilteringMode(Filtering.FILTERINGMODE_OFF);
  address.setImmediate(true)
  address.setNewItemsAllowed(true);
  address.setNullSelectionAllowed(true);
  address.setNewItemHandler(new NewItemHandler{
    def addNewItem(newItemCaption: String) = {
      if(addresses.find{_("address") == newItemCaption}.isEmpty) {
        val row = addresses.addItem
        addresses.update(row, "address", newItemCaption)
        addresses.update(row, "id", 0)
        address.setValue(row)
      }
    }
  })


  val comment : TextField = new TextField("Комментарии:")
  comment.setRows(5)
  comment.setWidth("100%")

  val orgs : MyIndexedContainer = new MyIndexedContainer
  orgs.addContainerProperty("id", classOf[java.lang.Integer], 0)
  orgs.addContainerProperty("name", classOf[String], "")
  orgs.addContainerProperty("employment", classOf[java.lang.Integer], "")
  Cloud.query("CurrentPersonOrganizations").head.*("employments").foreach { e : DataMap =>
    val row = orgs.addItem
    orgs.update(row, "id", e./("organization").getId)
    orgs.update(row, "name", e./("organization").!("name"))
    orgs.update(row, "employment", e.getId)
  }
  organization.setContainerDataSource(orgs)
  organization.setItemCaptionPropertyId("name")
  organization.addListener(new ValueChangeListener{
    def valueChange(event: ValueChangeEvent) = {
      loadAddress
    }
  })
  organization.setValue(orgs.firstItemId)

  var addresses : MyIndexedContainer = _
  def loadAddress = {
    addresses = new MyIndexedContainer
    addresses.addContainerProperty("id", classOf[java.lang.Integer], 0)
    addresses.addContainerProperty("address", classOf[String], "")
    Cloud.find("Organization", orgs.getId(organization.getValue), List("id", "addresses")).*("addresses").foreach { m : DataMap =>
      val row = addresses.addItem
      addresses.update(row, "id", m.getId)
      addresses.update(row, "address", m("address"))
    }
    address.setContainerDataSource(addresses)
    address.setItemCaptionPropertyId("address")
  }


  val bid : MyButton = MyButton("Оформить заказ") {e : MyClickEvent =>
    var a : Any = null
    if(address.getValue != null) {
      val addressId  = addresses.getId(address.getValue)
      if(addressId != 0) {
        a = addressId
      } else {
        a = addresses(address.getValue, "address")
      }
    }

    Cloud.action("GenerateBidFromCart",
      Map("rem" -> comment.getValue,
        "organization" -> orgs.getId(organization.getValue),
        "employment" -> orgs(organization.getValue, "employment"),
        "address" -> a))
    Cloud.action("ClearMyCart")
    var par : Window = this.getParent.asInstanceOf[Window]
    par.showNotification("Заказ оформлен", Notification.TYPE_HUMANIZED_MESSAGE)
    par.removeWindow(this)
  }

  setFullContent(
    vertical.margin addExpanded {
      form.add{organization}.add{address}.add{comment}
    } add {
      bid
    }
    )
}