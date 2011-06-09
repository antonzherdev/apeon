package ru.apeon.core.loader

import javax.servlet.{ServletContextEvent, ServletContextListener}

/**
 * @author Anton Zherdev
 */

class LoaderListener extends ServletContextListener {
  def contextDestroyed(p1: ServletContextEvent) {
    Loader.unload()
  }

  def contextInitialized(p1: ServletContextEvent) {
    Loader.load()
  }
}