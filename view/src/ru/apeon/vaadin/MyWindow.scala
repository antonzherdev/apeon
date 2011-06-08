package ru.apeon.vaadin

import com.vaadin.ui.{ComponentContainer, Window}

/**
 * @author Anton Zherdev
 */

abstract class MyWindow extends Window with Vaadin {
  def this(caption : String) = {
    this()
    setCaption(caption)
  }


  def setFullContent(newContent: ComponentContainer) = {
    setContent(newContent)
    newContent.setSizeFull
  }
}