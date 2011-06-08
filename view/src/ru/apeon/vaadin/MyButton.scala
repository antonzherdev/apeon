package ru.apeon.vaadin

import com.vaadin.ui.{CheckBox, Button, Component}

/**
 * @author Anton Zherdev
 */

trait MyClickListener {
  def buttonClick(event: MyClickEvent): Unit
}

object MyButton {
  def apply(text : String)(listener : MyClickEvent => Unit) : MyButton ={
    val ret : MyButton = new MyButton
    ret.setCaption(text)
    ret.addListener(listener)
    ret
  }
}

class MyButton extends Button {
  val listeners : ListenerManager[MyClickEvent] = new ListenerManager

  override def fireClick = {
    super.fireClick
    listeners.fireEvent(new MyClickEvent(MyButton.this))
  }

  def addListener(listener: MyClickEvent => Unit): Unit = {
    listeners.addListener(listener)
  }

  def removeListener(listener: MyClickEvent => Unit): Unit = {
    listeners.removeListener(listener)
  }
}

abstract class MyEvent{
  val source : Component
}

class MyClickEvent(val source : Button) extends MyEvent
class MyCheckBoxEvent(val source : MyCheckBox, val value : Boolean) extends MyEvent

object MyCheckBox {
  def apply(text : String)(listener : MyCheckBoxEvent => Unit) : MyCheckBox ={
    val ret : MyCheckBox = new MyCheckBox
    ret.setCaption(text)
    ret.addListener(listener)
    ret.setImmediate(true)
    ret
  }
}

class MyCheckBox extends CheckBox {
  val listeners : ListenerManager[MyCheckBoxEvent] = new ListenerManager

  override def fireClick = {
    super.fireClick
    listeners.fireEvent(new MyCheckBoxEvent(MyCheckBox.this, MyCheckBox.this.getValue.asInstanceOf[Boolean]))
  }

  def addListener(listener: MyCheckBoxEvent => Unit): Unit = {
    listeners.addListener(listener)
  }

  def removeListener(listener: MyCheckBoxEvent => Unit): Unit = {
    listeners.removeListener(listener)
  }
}