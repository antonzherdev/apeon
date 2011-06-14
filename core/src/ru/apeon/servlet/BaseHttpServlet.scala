package ru.apeon.servlet

import akka.util.Logging
import ru.apeon.core.loader.Loader
import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}

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

  def tr(resp: HttpServletResponse)( f : => Unit) {
    if(Loader.loaderError.isDefined) {
      resp.sendError(500, "Error in loading: %s".format(Loader.loaderError.get.getMessage))
    }
    else {
      try {
        f
      }
      catch {
        case e : Throwable => {
          log.error(e, "Servlet")
          resp.sendError(500, e.getMessage)
        }
        case _ => {
          log.error("Servlet")
          resp.sendError(500)
        }
      }
    }
  }
}
