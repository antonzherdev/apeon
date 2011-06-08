package ru.apeon.mail

import com.vaadin.ui._
import com.vaadin.terminal.ThemeResource
import ru.apeon.vaadin._

/**
 * @author Anton Zherdev
 */

class EMailApp extends MyApplication with Vaadin {
  def init = {
    setMainWindow(new EMailWindow)
  }
}

class BoxLabel(name : String, icon : String) extends Vaadin {
  val component = horizontal.spacing.addStyleName("m-email-box-label").add{
    new Embedded("", new ThemeResource("../comtec/mail/" + icon + ".png"))
  }.addExpanded{
    val label = new Label(name)
    label
  }.align(Left, Center).setWidth("100%").layout
}

class EMailWindow extends MyWindow("Электронная почта") {
  val inbox = new BoxLabel("Входящие", "inbox")
  val draft = new BoxLabel("Черновики", "draft")
  val sent = new BoxLabel("Отправленные", "sent")
  val trash  = new BoxLabel("Корзина", "trash")

  val folders : Tree = new Tree
  val messages : MyTable = new MyTable

  val subject : Label = new Label
  val message : Label = new Label
  val from : Label = new Label
  val to : Label = new Label
  val when : Label = new Label
  val searchField : SearchField = SearchField{ e =>

  }
  setContent(
    toolbar {
        absolute.setHeight("35px").add("top : 5px; left : 10px") {
          searchField
        }
    } body {
      horizontalSplit {
        horizontalSplit {
          vertical.add{inbox}.add{draft}.add{sent}.add{trash}/*.add{
            new Label("Папки")
          }*/.addExpanded{
            folders
          }
        }.split{
           messages
        }
      }.split {
        vertical.add{
          vertical.margin.add {
            subject
          }.add {
            horizontal.add{new Label("From ")}.add{from}.add{new Label(" to ")}.add{to}.add{when}
          }
        }.add{
          message
        }
      }
    }
    )

  implicit def toComponent(l : BoxLabel) : Component = l.component
}