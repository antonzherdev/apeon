package ru.apeon.cloud

import org.apache.commons.httpclient.auth.{AuthScope, AuthPolicy}
import org.springframework.security.core.context.SecurityContextHolder
import ru.apeon.auth.ProxyUserDetails
import java.io.IOException
import org.apache.commons.httpclient.methods.{DeleteMethod, StringRequestEntity, PostMethod, GetMethod}
import org.apache.commons.httpclient._
import org.springframework.security.web.authentication.rememberme.AbstractRememberMeServices

/**
 * @author Anton Zherdev
 */

object Cloud {
  def select(name : String) : List[DataMap] = select(name, List())


  def select(name : String, columns : List[String]) : List[DataMap]  = {
    CloudSer.toList(
      get("http://localhost:8080/comtec-cloud/data/" + name,
        Map("columns" -> columnsParameter(columns))).getResponseBodyAsStream
      )

  }

  def find(name : String, id : Int) : DataMap = find(name, id, List())

  def find(name : String, id : Int, columns : List[String]) : DataMap  = {
    CloudSer.toMap(get("http://localhost:8080/comtec-cloud/data/" + name, Map("id" -> id, "columns" -> columnsParameter(columns))).getResponseBodyAsStream)
  }

  def delete(name : String, id : Int)  = {
    val method : DeleteMethod = new DeleteMethod(buildUrl("http://localhost:8080/comtec-cloud/data/" + name, Map("id" -> id)))

    runMethod(method)
  }

  def insert(name : String, values : collection.Map[String, Any]) : String =
    post("http://localhost:8080/comtec-cloud/data/" + name, values).getResponseBodyAsString

  def update(name : String, id : Int, values : collection.Map[String, Any]) : String =
    post("http://localhost:8080/comtec-cloud/data/" + name + "?id=" + id, values).getResponseBodyAsString

  private def columnsParameter(columns : List[String]) : String = columns match {
    case List(x) => x
    case x :: xs => x + "," + columnsParameter(xs)
    case _ => ""
  }

  private def buildUrl(url : String, parameters : collection.Map[String, Any]) = url + "?" + parameters.foldLeft("") { (s, kv) =>
    val (key, value) = kv
    s + (if(!s.isEmpty) "&" else "" ) + key + "=" + value.toString
  }

  private def get(url : String) : HttpMethod = get(url, Map())

  private def get(url : String, parameters : collection.Map[String, Any]) : HttpMethod = {
    val method : GetMethod = new GetMethod(buildUrl(url, parameters))

    runMethod(method)
  }

  private def post(url : String, parameters : collection.Map[String, Any]) : HttpMethod  = {
    val method : PostMethod = new PostMethod(url)
    //TODO: !!Оживить
//    method.setRequestEntity(new StringRequestEntity(CloudSer.toString(parameters), "application/xml", "utf8"))

    runMethod(method)
  }

  private def runMethod(method : HttpMethod) : HttpMethod = {
    val client : HttpClient = new HttpClient

    client.getParams.setParameter(AuthPolicy.AUTH_SCHEME_PRIORITY, List(AuthPolicy.DIGEST, AuthPolicy.BASIC));
    client.getParams.setAuthenticationPreemptive(true);

    SecurityContextHolder.getContext.getAuthentication.getPrincipal match {
      case u : ProxyUserDetails => {
        if(!u.realPassword.isEmpty) {
          client.getState.setCredentials(
            new AuthScope("localhost", 8080, AuthScope.ANY_REALM),
            new UsernamePasswordCredentials(u.getUsername, u.realPassword));
        }
        if(!u.proxy.isEmpty) {
          client.getState.addCookie(new Cookie(
            "localhost",
            AbstractRememberMeServices.SPRING_SECURITY_REMEMBER_ME_COOKIE_KEY,
            u.proxy,
            "/comtec-cloud",
            null,
            false))
        }
      }
      case _ =>  throw new CloudError("Authentification error")
    }

    try{
      if(client.executeMethod(method) != 200) {
        throw new CloudError("Response code != 200")
      }
    }
    catch {
      case e : IOException => throw new CloudError("IO error", e)
      case e : HttpException => throw new CloudError("Http error", e)
    }

    method
  }

  def action(name : String) : String = action(name, Map())

  def action(name : String, parameters : collection.Map[String, Any]) : String =
    post("http://localhost:8080/comtec-cloud/action/" + name, parameters).getResponseBodyAsString

  def query(name : String) : List[DataMap] = query(name, Map.empty)
  def query(name : String, parameters : collection.Map[String, Any]) : List[DataMap] =
    CloudSer.toList(post("http://localhost:8080/comtec-cloud/query/" + name, parameters).getResponseBodyAsStream)
}


