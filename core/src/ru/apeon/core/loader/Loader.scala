package ru.apeon.core.loader

import java.io.File
import ru.apeon.core.script.{DefaultObjectModel, ObjectModel}
import xml.XML
import ru.apeon.core.entity.EntityConfiguration

object Loader {
  def load() {
    val tomcatFolder = System.getProperty("catalina.base")
    val apeonFolder = new File(tomcatFolder + "/apeon")
    if(!apeonFolder.exists) throw LoaderException("Folder \"%s\" doesn`t exists. Nothing to deploy.".format(apeonFolder.getAbsolutePath))
    val model = new DefaultObjectModel
    EntityConfiguration.model = model
    val modules = apeonFolder.listFiles.map{dir =>
      loadModule(model, dir)
    }

    ScriptLoader.load(model, modules.map{module => new File(module.path + "/apeon")}.filter(_.isDirectory))
  }

  def loadModule(model : ObjectModel, dir : File) : Module = {
    val xml = XML.loadFile(dir.getAbsolutePath + "/module.xml")

    Module(
      name = (xml\"name").text,
      version = (xml\"version").text.split('.').toSeq.map{_.toInt},
      path = dir.getAbsolutePath
    )
  }
}

case class LoaderException(message : String) extends RuntimeException(message)