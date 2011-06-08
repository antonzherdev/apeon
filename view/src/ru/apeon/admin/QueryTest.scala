package ru.apeon.admin

import com.vaadin.ui.{Form, Window, TextField}
import ru.apeon.cloud.Cloud
import collection._
import ru.apeon.vaadin.{MyApplication, MyClickEvent, MyButton, Vaadin}

/**
 * @author Anton Zherdev
 */

class QueryTest extends MyApplication with Vaadin {
  val nameField : TextField = new TextField("Запрос")
  nameField.setWidth("100%")
  val paramField : TextField = new TextField("Параметры")
  paramField.setWidth("100%")
  paramField.setRows(3)

  val f : Form = new Form

  f.getLayout.addComponent(nameField)
  f.getLayout.addComponent(paramField)

  val run : MyButton = MyButton("Пуск") { e : MyClickEvent =>
    val sb : StringBuilder = new StringBuilder

    val pars : mutable.Map[String, Any] = mutable.Map.empty
    for(line : String <- paramField.getValue.toString.split("\n")) {
      val pos = line.indexOf("=")
      if(pos > 0) {
        pars += (line.substring(0, pos).trim -> line.substring(pos + 1).trim)
      }
    }
    Cloud.query( nameField.getValue.toString, pars ).foreach{ m =>
      sb.append(m)
      sb.append('\n')
    }
    result.setValue(sb.toString)
  }
  f.setFooter(horizontal add{run});

  val result : TextField = new TextField()
  result.setWidth("100%")
  result.setRows(30)

  def init = {
    setMainWindow(new Window("Тест запросов",
      vertical.margin addExpanded{
        f
      }add {
        result
      }))
  }
}