package ru.apeon.core.loader

import java.io.File
import akka.util.Logging
import java.net.{URLClassLoader, URL}
import xml.{NodeSeq, XML}
import ru.apeon.core.script.{Module, DefaultObjectModel, ObjectModel}
import ru.apeon.core.entity.{EntityConfiguration}

object Loader extends Logging {
  var modules : Seq[Module] = Seq()
  var listeners : Seq[Listener] = Seq()
  var classLoader : URLClassLoader = _
  private var _apeonXml : NodeSeq = _
  private var _model : ObjectModel = new DefaultObjectModel
  def apeonXml = _apeonXml

  def model = _model

  def load() {
    unload()
    log.info("Loading modules")
    val tomcatFolder = System.getProperty("catalina.home")
    _apeonXml = XML.loadFile(tomcatFolder + "/conf/apeon.xml")

    val apeonFolder = new File(tomcatFolder + "/apeon")
    EntityConfiguration.model = model

    val modulesBuilder = Seq.newBuilder[Module]
    if(apeonFolder.exists) {
      apeonFolder.listFiles.par.foreach{dir =>
        modulesBuilder += loadModule(model, dir)
      }
    }
    apeonXml.\\("module").par.foreach{module =>
      modulesBuilder += loadModule(model, new File(module.\("@dir").text))
    }

    modules = modulesBuilder.result()

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

    ScriptLoader.load(model, modules)

    loadListeners()
    model.dataSources.foreach(_.load())

    log.info("Loaded modules")
  }

  def unload() {
    log.info("Unloading modules")
    listeners.foreach{listener =>
      listener.unload()
    }
    model.dataSources.foreach(_.unload())
    listeners = Seq()
    _model = new DefaultObjectModel
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