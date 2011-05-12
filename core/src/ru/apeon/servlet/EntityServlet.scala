package ru.apeon.servlet

import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import ru.apeon.core.entity._
import ru.apeon.core.eql._

class EntityServlet extends BaseHttpServlet {
  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    tr {
      redeploy(req)

      val entityDescription = EntityConfiguration.model.entityDescription(entityName(req))
      val columns: Seq[Column] = req.getParameter("columns") match {
        case null => Seq()
        case "" => Seq()
        case p: String => p.split(",").toSeq.map {
          column =>
            val prop = entityDescription.fieldOption(column).get
            Column(Ref(prop.name), prop.name)
        }
      }

      resp.setCharacterEncoding("utf8")
      resp.setContentType("application/xml")

      resp.getWriter.print(
        entityDescription.dataSource.store.select(
          Select(
            columns = columns,
            from = FromEntity(entityDescription))).toString())
    }
  }

  def entityName(req: HttpServletRequest) : String = req.getRequestURI.split("/").last

}
