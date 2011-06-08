package ru.apeon.sync.buffer

import xml.XML
import ru.apeon.core.loader.{Loader, Listener}
import java.util.{TimerTask, Timer}
import ru.apeon.core.script._
import akka.util.Logging
import java.io.File

/**
 * @author Anton Zherdev
 */

class BufferListener extends Listener with Logging{
  val timer = new Timer
  var model : ObjectModel = _
  def unload(model: ObjectModel) {
    timer.cancel()
  }

  def preLoad(model: ObjectModel) {

  }

  def load(model: ObjectModel) {
    this.model = model
    val tomcatFolder = System.getProperty("catalina.home")
    val file = new File(tomcatFolder + "/conf/sync.xml")
    if(file.exists) {
      val xml = XML.loadFile(file)
      val xmlBuffers = (xml\\"buffer")


      model.objs.filter(_.isInstanceOf[Buffer]).foreach {case buffer : Buffer =>
        val xmlBuffer = xmlBuffers.find(b => (b\"@name").text == buffer.fullName).getOrElse {
          throw new RuntimeException("Buffent %s not found in sync.xml".format(buffer.name))
        }

        val impl = Loader.newInstance((xmlBuffer\"@class").text).asInstanceOf[BufferImpl]
        impl.init((xmlBuffer\"@url").text)

        timer.schedule(Task(buffer, impl), 100, (xmlBuffer\"@frequency").headOption.map{_.text.toInt}.getOrElse{60000})
        log.info("Init buffer %s".format(buffer.name))
      }
    }
  }

  case class Task(buffer : Buffer, impl : BufferImpl) extends TimerTask {
    def run() {
      log.debug("Checking buffer %s".format(buffer.name))
      buffer.synchronized{
        impl.items.foreach{item =>
          log.info("Loading buffer item %s from buffer %s".format(item.name, buffer.name))
          val e = new DefaultEnvironment(model)
          e.start()
          buffer.applyDef.value(e, Some(Seq(ParVal(item.name, None), ParVal(item.stream, None))), None)
          e.end()
          item.end()
          log.info("Loaded buffer item %s from buffer %s".format(item.name, buffer.name))
        }
      }
    }
  }
}