package ru.apeon.servlet

import javax.servlet.http.{HttpServletRequest, HttpServlet}
import akka.util.Logging
import ru.apeon.core.loader.Loader

/**
 * @author Anton Zherdev
 */

class BaseHttpServlet extends HttpServlet with Logging {
  def redeploy(req: HttpServletRequest) {
    if(req.getParameter("redeploy") == "true") {
      Loader.synchronized{
        Loader.load()
      }
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
