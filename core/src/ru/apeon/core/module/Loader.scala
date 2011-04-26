package ru.apeon.core.module

import java.io.File
import ru.apeon.core.script.{DefaultObjectModel, ObjectModel}
import xml.XML

object Loader {
  def load() {
    val tomcatFolder = System.getProperty("catalina.base")
    val apeonFolder = new File(tomcatFolder + "/apeon")
    if(!apeonFolder.exists) throw LoaderException("Folder \"%s\" doesn`t exists. Nothing to deploy.".format(apeonFolder.getAbsolutePath))
    val model = new DefaultObjectModel
    apeonFolder.listFiles.foreach{dir =>
      loadModule(model, dir)
    }
  }

  def loadModule(model : ObjectModel, dir : File) : Module = {
    val xml = XML.loadFile(dir.getAbsolutePath + "/module.xml")

    Module(
      name = (xml\"name").text,
      version = (xml\"version").text.split('.').toSeq.map{_.toInt}
    )
  }
}

case class LoaderException(message : String) extends RuntimeException(message)