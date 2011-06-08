package ru.apeon.vaadin

import com.vaadin.ui.TextField
import com.vaadin.event.ShortcutAction.{ KeyCode}
import com.vaadin.event.FieldEvents.{BlurEvent, BlurListener, FocusEvent, FocusListener}

/**
 * @author Anton Zherdev
 */
object SearchField{
  def apply(f : SearchEvent => Unit) : SearchField = {
    val ret : SearchField = new SearchField
    ret.addListener(f)
    ret
  }
}


class SearchField extends TextField {
  setInputPrompt("Поиск")
  setWidth("200px")
  setImmediate(true)

  /*var focused : Boolean = false
  addListener(new FocusListener{
    def focus(event: FocusEvent) = {focused = true}
  })

  addListener(new BlurListener{
    def blur(event: BlurEvent) = {focused = false}
  })*/

  val listeners : ListenerManager[SearchEvent] = new ListenerManager
  addShortcutListener(new MyShortcutListener("Enter", KeyCode.ENTER) {
    def handleAction(sender: Any, target: Any) = //if(focused){
      listeners.fireEvent(new SearchEvent(SearchField.this, getValue.toString))
//    }
  })

  def addListener(listener: SearchEvent => Unit): Unit = {
    listeners.addListener(listener)
  }

  def removeListener(listener: SearchEvent => Unit): Unit = {
    listeners.removeListener(listener)
  }
}

class SearchEvent(val source : SearchField, val text : String) extends MyEvent