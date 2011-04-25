package ru.apeon.desktop

import ru.apeon.vaadin._
import com.vaadin.ui.{Label, Embedded}
import com.vaadin.event.MouseEvents.{ClickEvent, ClickListener}
import com.vaadin.terminal.{ExternalResource, ThemeResource}

/**
 * @author Anton Zherdev
 */

class Desktop extends MyApplication with Vaadin {
  def init = {
    setMainWindow(new DesktopWindow)
  }
}

class DesktopWindow extends MyWindow("Рабочий стол") {
  setFullContent {
    grid(10, 10).margin.spacing.add{
      new DesktopItem("Order", "Формирование заявки", "app/cart.png").component
    }.add {
      new DesktopItem("Bids", "Мои заявки", "app/cart.png").component
    }
  }
}

class DesktopItem(app : String, text : String, img : String) extends Vaadin{
  val image = new Embedded("", new ThemeResource(img))
  val label = new Label(<center>{text}</center>.toString, Label.CONTENT_XHTML)

  image.addListener(new ClickListener{
    def click(event: ClickEvent) = {
      open
    }
  })

  def open = {
    image.getApplication.getMainWindow.open(new ExternalResource("./" + app + "/"))
  }

  def component =
    vertical.spacing.add{
       image
    }.align(Center, Top).add{
      label
    }.align(Center, Top).layout

}