package ru.apeon.c1

import ru.apeon.core.entity.{SqlPersistentStoreBase}
import java.sql.DriverManager
import ru.apeon.core.eql.{Select, SqlGenerator}
import collection.Map
import com.ipc.oce.objects.OCCatalogRef

class C1PersistentStore(val name : String, val url : String, val userName : String, val password : String)
        extends SqlPersistentStoreBase
{
  def getConnection = {
    Class.forName("com.ipc.oce.jdbc.OCEDriver")
    DriverManager.getConnection(url, userName, password)
  }
  val generator = new SqlGenerator
  val dialect = new C1SqlDialect

  override def select(select: Select, parameters: Map[String, Any]) = e.transaction{
    super.select(select, parameters).map{row =>
      row.map{
        case (name, ref : OCCatalogRef) => (name, ref.getUUID.toString)
        case o => o
      }
    }
  }
}