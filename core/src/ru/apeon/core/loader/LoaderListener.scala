package ru.apeon.core.loader

import javax.servlet.{ServletContextEvent, ServletContextListener}
import akka.util.Logging

/**
 * @author Anton Zherdev
 */

class LoaderListener extends ServletContextListener with Logging  {
  def contextDestroyed(p1: ServletContextEvent) {
    Loader.unload()
  }

  def contextInitialized(p1: ServletContextEvent) {
    try {
      Loader.load()
    }
    catch {
      case e : Throwable => {
        log.error(e, "Loader")
      }
    }
  }
}