package ru.apeon.servlet

import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import ru.apeon.core.entity._

class QueryServlet  extends BaseHttpServlet {
  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    tr {
      redeploy(req)

      val b = Map.newBuilder[String, String]
      val i = req.getParameterMap.entrySet.iterator
      while(i.hasNext) {
        val e = i.next
        b += ((e.getKey.toString, e.getValue.asInstanceOf[Array[String]].mkString(", ")))
      }
      val pars = b.result().filter(_._1 != "redeploy")
      ans(req, resp, pars)
    }
  }

  override def doPost(req: HttpServletRequest, resp: HttpServletResponse) {
    tr {
      //TODO: параметры запроса
      ans(req, resp /*,CloudSer.toMutableMap(req.getInputStream)*/)
    }
  }

  def ans(req: HttpServletRequest, resp: HttpServletResponse, pars : Map[String, String] = Map()) {
    resp.setCharacterEncoding("utf8")
    resp.setContentType("text/plain")
    val query = EntityConfiguration.model.obj(queryName(req)).asInstanceOf[Query]
    val ans = query.execute(pars).toString
    resp.getWriter.print(ans)
  }

  def queryName(req: HttpServletRequest) : String = req.getRequestURI.split("/").last
}