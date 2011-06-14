package ru.apeon.servlet

import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import ru.apeon.core.entity._
import org.springframework.web.multipart.commons.CommonsMultipartResolver

class QueryServlet  extends BaseHttpServlet {
  def a(req: HttpServletRequest, resp: HttpServletResponse) {
    val b = Map.newBuilder[String, Any]
    val i = req.getParameterMap.entrySet.iterator
    while (i.hasNext) {
      val e = i.next
      b += ((e.getKey.toString, e.getValue.asInstanceOf[Array[String]].mkString(", ")))
    }
    val c = new CommonsMultipartResolver(getServletContext)
    if(c.isMultipart(req)) {
      val i = c.resolveMultipart(req).getFileMap.values().iterator()
      while(i.hasNext) {
        val file = i.next
        b += ((file.getName, file.getInputStream))
      }
    }
    val pars = b.result().filter(_._1 != "redeploy")
    ans(req, resp, pars)
  }

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    tr(resp) {
      redeploy(req)

      a(req, resp)
    }
  }

  override def doPost(req: HttpServletRequest, resp: HttpServletResponse) {
    tr(resp) {
      a(req, resp)
    }
  }

  def ans(req: HttpServletRequest, resp: HttpServletResponse, pars : Map[String, Any] = Map()) {
    resp.setCharacterEncoding("utf8")
    resp.setContentType("text/plain")
    val query = EntityConfiguration.model.obj(queryName(req)).asInstanceOf[Query]
    val ret = query.execute(pars)
    if(ret != null) {
      resp.getWriter.print(ret.toString)
    } else {
      resp.getWriter.print("null")
    }
  }

  def queryName(req: HttpServletRequest) : String = req.getRequestURI.split("/").last
}