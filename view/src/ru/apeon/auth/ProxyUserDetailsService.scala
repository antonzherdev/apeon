package ru.apeon.auth

import java.lang.String
import org.apache.commons.httpclient.methods.GetMethod
import java.io.IOException
import org.apache.commons.httpclient.{HttpException, HttpClient}
import org.springframework.security.core.userdetails.{UserDetails, UsernameNotFoundException, UserDetailsService}
import org.springframework.security.core.authority.GrantedAuthorityImpl
import org.springframework.security.authentication.dao.{AbstractUserDetailsAuthenticationProvider}
import org.springframework.security.authentication.{BadCredentialsException, AuthenticationServiceException, UsernamePasswordAuthenticationToken}
import java.security.MessageDigest
import java.math.BigInteger
import org.springframework.security.web.authentication.rememberme.TokenBasedRememberMeServices
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import org.springframework.security.core.{GrantedAuthority}

/**
 * @author Anton Zherdev
 */

class ProxyUserDetailsService extends UserDetailsService {
  def loadUserByUsername(userName: String) = {
    var method : GetMethod = new GetMethod("http://localhost:8080/comtec-cloud/user/" + userName)
    var client : HttpClient = new HttpClient
    try{
      if(client.executeMethod(method) != 200) {
        throw new UsernameNotFoundException("Response code != 200")
      }
      new ProxyUserDetails(userName, method.getResponseBodyAsString)
    }
    catch {
      case e : IOException => throw new UsernameNotFoundException("IO error", e)
      case e : HttpException => throw new UsernameNotFoundException("Http error", e)
    }
  }
}

class ProxyUserDetails(val userName : String, val password : String) extends UserDetails{
  var realPassword : String = ""
  var proxy : String = ""

  def isEnabled = true

  def isCredentialsNonExpired = true

  def isAccountNonLocked = true

  def isAccountNonExpired = true

  def getUsername = userName

  def getPassword = password

  val auth : java.util.Collection[GrantedAuthority] = new  java.util.ArrayList[GrantedAuthority]
  auth.add(new GrantedAuthorityImpl("ROLE_USER"))

  def getAuthorities = auth
}

class ProxyAuthenticationProvider extends AbstractUserDetailsAuthenticationProvider {
  val service : UserDetailsService = new ProxyUserDetailsService

  def retrieveUser(userName: String, auth: UsernamePasswordAuthenticationToken) = service.loadUserByUsername(userName) match {
    case p : ProxyUserDetails => {
      p.realPassword = auth.getCredentials.toString
      p
    }
    case _ => throw new  AuthenticationServiceException("Error")
  }

  def additionalAuthenticationChecks(ud: UserDetails, p: UsernamePasswordAuthenticationToken) = {
    val pas : String = p.getCredentials.toString
    val m : MessageDigest = MessageDigest.getInstance("MD5");
    m.update(pas.getBytes(),0,pas.length());
    if(new BigInteger(ud.getPassword, 16) != new BigInteger(1,m.digest())) {
      throw new BadCredentialsException("Wrong password")
    }
  }
}

class ProxyRememberMeService extends TokenBasedRememberMeServices {

  override def processAutoLoginCookie(cookieTokens: Array[String], request: HttpServletRequest, response: HttpServletResponse) = {
    val p : ProxyUserDetails = super.processAutoLoginCookie(cookieTokens, request, response).asInstanceOf[ProxyUserDetails]
    p.proxy = extractRememberMeCookie(request)
    p
  }
}