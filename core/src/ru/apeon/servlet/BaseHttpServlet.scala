package ru.apeon.servlet

import ru.apeon.core.entity._
import javax.servlet.http.{HttpServletRequest, HttpServlet}
import akka.util.Logging
import ru.apeon.core.script.{DefaultObjectModel, ScriptLoader}

/**
 * @author Anton Zherdev
 */

class BaseHttpServlet extends HttpServlet with Logging {
  private var loadAtThisTime = false
  override def init() {
    tr {
      loadAtThisTime = true
      Script.load()
    }
  }

  def redeploy(req: HttpServletRequest) {
    if(!loadAtThisTime) {
      if(req.getParameter("redeploy") == "true") {
        ScriptLoader.synchronized{
          Script.isLoad = false
          Script.load()
        }
      }
    } else {
      loadAtThisTime = false
    }
  }

  def tr( f : => Unit) {
    try {
      f
    }
    catch {
      case e : Throwable => {
        log.error(e, "Servlet")
        throw e
      }
      case _ => log.error("Servlet")
    }
  }
}

object Script{
  var isLoad = false
  def load() {
    if(!isLoad) {
      ScriptLoader.synchronized{
        if(!isLoad) {
          EntityConfiguration.model = new DefaultObjectModel
          ScriptLoader.load()
          isLoad = true
        }
      }
    }
  }
}