package ru.apeon.core.loader

import java.io.File
import ru.apeon.core.script.{DefaultObjectModel, ObjectModel}
import ru.apeon.core.entity.EntityConfiguration
import akka.util.Logging
import java.net.{URLClassLoader, URL}
import xml.{NodeSeq, XML}

object Loader extends Logging {
  var modules : Seq[Module] = Seq()
  var listeners : Seq[Listener] = Seq()
  var classLoader : URLClassLoader = _
  private var _apeonXml : NodeSeq = _

  def apeonXml = _apeonXml

  def load() {
    try{
      unload()
      log.info("Loading modules")
      val tomcatFolder = System.getProperty("catalina.home")
      _apeonXml = XML.loadFile(tomcatFolder + "/conf/apeon.xml")

      val apeonFolder = new File(tomcatFolder + "/apeon")
      if(!apeonFolder.exists) throw LoaderException("Folder \"%s\" doesn`t exists. Nothing to deploy.".format(apeonFolder.getAbsolutePath))
      val model = new DefaultObjectModel
      EntityConfiguration.model = model
      modules = apeonFolder.listFiles.map{dir =>
        loadModule(model, dir)
      }

      val urls = Array.newBuilder[URL]
      for(module <- modules) {
        val classes = new File(module.path + "/classes")
        if(classes.isDirectory) {
          urls += classes.toURI.toURL
        }

        val lib = new File(module.path + "/lib")
        if(lib.isDirectory) {
          lib.listFiles.filter(_.getName.endsWith(".jar")).foreach{jar =>
            urls += jar.toURI.toURL
          }
        }
      }

      classLoader = new URLClassLoader(urls.result(), getClass.getClassLoader)

      ScriptLoader.load(model, modules.map{module => new File(module.path + "/apeon")}.filter(_.isDirectory))

      loadListeners()

      log.info("Loaded modules")
    }
    catch {
      case e : Throwable => {
        log.error(e, "Loader")
      }
    }
  }

  def unload() {
    log.info("Unloading modules")
    listeners.foreach{listener =>
      listener.unload()
    }
    listeners = Seq()
    log.info("Unloaded modules")
  }

  def loadListeners() {
    val listenersBuilder = Seq.newBuilder[Listener]
    for (module <- modules) {
      for (listener <- module.listeners) {
        listenersBuilder += classLoader.loadClass(listener).newInstance.asInstanceOf[Listener]
      }
    }
    listeners = listenersBuilder.result()

    log.info("Loading listeners")
    listeners.foreach {
      listener =>
        log.info("Loading %s".format(listener.getClass.getName))
        listener.load()
        log.info("Loaded %s".format(listener.getClass.getName))
    }
    log.info("Loaded listeners")
  }

  def loadModule(model : ObjectModel, dir : File) : Module = {
    val xml = XML.loadFile(dir.getAbsolutePath + "/module.xml")

    Module(
      name = (xml\"name").text,
      version = (xml\"version").text.split('.').toSeq.map{_.toInt},
      path = dir.getAbsolutePath,
      listeners = (xml\"listener").map(_.text)
    )
  }

  def loadClass(className : String) : Class[_] = classLoader.loadClass(className)

  def newInstance(className : String) : Any = loadClass(className).newInstance
}

case class LoaderException(message : String) extends RuntimeException(message)