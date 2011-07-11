package ru.apeon.c1

import java.io.File
import java.sql.{DriverManager}
import com.ipc.oce._
import objects._
import org.jinterop.dcom.core.JIVariant
import java.util.{UUID, Properties}

/**
 * @author Anton Zherdev
 */

object Test extends App with ConfigurationConstants{
  System.out.println("TEST")

  val configuration = new Properties
  configuration.setProperty("oce.driver", "V81Driver");
  configuration.setProperty("oce.host", "192.168.100.100");
  configuration.setProperty("oce.host.user", "ttt");
  configuration.setProperty("oce.host.password", "z");
  configuration.setProperty("oce.1c.dbpath", "C:\\1c\\upp1");
  configuration.setProperty("oce.1c.user", "test");
  configuration.setProperty("oce.1c.password", "z");
  val driver = ApplicationDriver.loadDriver(configuration.get("oce.driver").asInstanceOf[String] )
  driver.setAutoRegistration(true) // только для самого первого подключения
  val app = OCApp.getNewInstance
  app.setApplicationDriver(driver);
  try{
    app.connect(configuration);
    val man = app.getCatalogManager("Номенклатура")
    val doc = man.createItem()
    doc.write()
    System.out.println("Computer name: "+app.getComputerName)
  }finally{
    app.exit()
  }

 /* Class.forName("com.ipc.oce.jdbc.OCEDriver")
  val con = DriverManager.getConnection("jdbc:oce:dcom://192.168.100.100:ttt@z;oce.1c.dbpath=C:\\1c\\upp1;oce.driver=V81Driver;autoRegistration=true", "test", "z")
  try{
    //val rs = con.getMetaData.getColumns(null, null, "Номенклатура", "")
    val stat = con.createStatement()
//    val rs = stat.executeQuery("SELECT Ref, Наименование FROM Catalog.Номенклатура")
//    stat.
    val rs = stat.executeQuery(
"""select
	t.Ref as id,
	t.Наименование as name,
	t.Артикул as number
from
	Catalog.Номенклатура as t
	where t.Ref = Выразить("6600a842-e9f1-11d8-8d32-505054503030" как СТРОКА)
"""
    )
    while(rs.next()){
//      val r = rs.getObject(1)
      System.out.println(rs.getString(1))
//6600a842-e9f1-11d8-8d32-505054503030

//      System.out.println(rs.getString("COLUMN_NAME"))
    }
//    stat.close()
    rs.close()

  }
  finally{
    if(con!=null) con.close()
  }*/
}