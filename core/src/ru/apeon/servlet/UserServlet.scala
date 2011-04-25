package ru.apeon.servlet

import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import org.springframework.web.context.support.WebApplicationContextUtils
import org.springframework.context.ApplicationContext
import org.springframework.security.core.userdetails.{UsernameNotFoundException, UserDetailsService}

/**
 * @author Anton Zherdev
 */

class UserServlet extends BaseHttpServlet {
  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    val context : ApplicationContext  = WebApplicationContextUtils.getWebApplicationContext(getServletContext);
    val service : UserDetailsService = context.getBean("userDetailsService") match {
      case u : UserDetailsService => u
      case _ => throw new SecurityException("Error")
    };
    val userName = req.getRequestURI.split("/").last
    resp.setCharacterEncoding("utf8")
    resp.setContentType("application/text")

    try{
      val pas : String = service.loadUserByUsername(userName).getPassword

      //      val m : MessageDigest = MessageDigest.getInstance("MD5");
      //      m.update(pas.getBytes(),0,pas.length());
      //
      //      resp.getWriter.print(new BigInteger(1,m.digest()).toString)
      resp.getWriter.print(pas)
    }
    catch {
      case e : UsernameNotFoundException => resp.sendError(404)
    }
  }

}