package ru.apeon.c1

import org.scalatest.FunSuite
import java.util.Properties
import org.scalatest.matchers.ShouldMatchers

/**
 * @author Anton Zherdev
 */

class URLParserTest extends FunSuite with ShouldMatchers{
  test("Parse") {
    val p = new Properties
    p.setProperty("oce.host", "192.168.100.100")
    p.setProperty("oce.host.user", "ttt")
    p.setProperty("oce.host.password", "z")
    p.setProperty("oce.1c.dbpath", "C:\\1c\\upp1")
    p.setProperty("oce.driver", "V81Driver")
    p.setProperty("autoRegistration", "true")

    C1URLParser(
      "jdbc:oce:dcom://192.168.100.100:ttt@z;" +
        "oce.1c.dbpath=C:\\1c\\upp1;oce.driver=V81Driver;autoRegistration=true") should equal(p)
//    C1URLParser(
//      "jdbc:oce:dcom://192.168.100.100:ttt@z;" +
//              "oce.1c.dbpath=C:\\1c\\upp1;oce.driver=V81Driver;autoRegistration=true") should equal(p)

  }
}