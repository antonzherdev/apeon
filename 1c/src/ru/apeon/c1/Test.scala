package ru.apeon.c1

import java.io.File
import com.ipc.oce.{OCApp, ApplicationDriver, PropertiesReader}
import java.sql.{DriverManager}
import com.ipc.oce.objects.{OCDocumentRef, OCCatalogRef}

/**
 * @author Anton Zherdev
 */

object Test extends Application{
  System.out.println("TEST")

  /*  val pr = new PropertiesReader(new File("C:\\apeon\\1c\\1c.properties"))
  val configuration = pr.getPropertiesForInstance("inst01")
  val driver = ApplicationDriver.loadDriver(configuration.get("oce.driver").asInstanceOf[String] )
  driver.setAutoRegistration(true) // только для самого первого подключения
  val app = OCApp.getNewInstance
  app.setApplicationDriver(driver);
  try{
    app.connect(configuration);


    val manager = app.getCatalogManager("Банки")
    val ref = manager.findByDescription("Г МОСКВА")
    val selection = manager.selectHierarchically(ref)
    while(selection.next.booleanValue){
      val level = selection.getLevelInSelection
      if(level == 0)
        System.out.println(selection.getCode + " " + selection.getDescription);
    }

    System.out.println("Computer name: "+app.getComputerName)
  }finally{
    app.exit()
  }*/

  Class.forName("com.ipc.oce.jdbc.OCEDriver")
  val con = DriverManager.getConnection("jdbc:oce:dcom://192.168.100.100:ttt@z;oce.1c.dbpath=C:\\1c\\upp1;oce.driver=V81Driver;autoRegistration=true", "test", "z")
  try{
    //val rs = con.getMetaData.getColumns(null, null, "Номенклатура", "")
    val stat = con.createStatement()
//    val rs = stat.executeQuery("SELECT Ref, Наименование FROM Catalog.Номенклатура")
    val rs = stat.executeQuery(
"""select
	t.Ref
from
	Document.ПоступлениеТоваровУслуг as t
"""
    )
    while(rs.next()){
      System.out.println(rs.getObject(1).asInstanceOf[OCDocumentRef].getObject.getTabularSection("Материалы") )

//      System.out.println(rs.getString("COLUMN_NAME"))
    }
//    stat.close()
    rs.close()

  }
  finally{
    if(con!=null) con.close()
  }
}