package ru.apeon.c1

import ru.apeon.core.entity.{SqlPersistentStoreBase}
import ru.apeon.core._
import eql.{Expression, ConstObject}
import script.Environment
import sql.Column
import akka.util.Logging
import com.ipc.oce.objects._OCCommonRef
import java.sql.{Connection, DriverManager}
import util.parsing.combinator.RegexParsers
import java.util.Properties

class C1PersistentStore(val name : String, val url : String, val userName : String, val password : String)
        extends SqlPersistentStoreBase with Logging
{
  val connection = {
    Class.forName("com.ipc.oce.jdbc.OCEDriver")
    DriverManager.getConnection(url, userName, password)
  }

  def getConnection = connection
  val generator = new C1SqlGenerator
  val dialect = new C1SqlDialect

  override protected val _eql = new Eql{
    override protected def sqlRow(select: eql.Select, sqlSelect: sql.Select, rows: sql.Rows) =
      new sql.RowSyntax(rows.rs, sqlSelect) {
        override def value(column: Column, j: Int) = {
          rs.getMetaData.getColumnTypeName(j + 1) match {
            case "JAVA_OBJECT" => C1Ref(rs.getString(j + 1), rs.getObject(j + 1).asInstanceOf[_OCCommonRef])
            case _ => super.value(column, j)
          }
        }
      }
  }

  override def closeConnection(connection: Connection) {
  }

  override def unload() {
    log.debug("Closing 1c connection.")
    connection.close()
  }
}

case class C1Ref(uuid : String, ref : _OCCommonRef) extends ConstObject{
  override def toString = uuid

  override def hashCode() = uuid.hashCode

  def expression = ConstRef(this)
}

case class ConstRef(ref : C1Ref) extends Expression {
  def dataType(env: Environment) = script.ScriptDataTypeString()
}

object C1URLParser {
  def apply(str : String) : Properties = {
    val p = new C1URLParser
    p.parse(p.phrase(p.url), str) match {
      case p.Success(ret, _) => ret
      case p.NoSuccess(err, _) => throw new RuntimeException(err)
    }
  }
}

class C1URLParser extends RegexParsers {
  protected def integer : Parser[Int] = """\d*""".r ^^ { _.toInt }

  protected def ip = integer ~ ("." ~> integer) ~ ("." ~> integer) ~ ("." ~> integer) ^^ {case i1 ~ i2 ~ i3 ~ i4 =>
    "%d.%d.%d.%d".format(i1, i2, i3, i4)
  }

  protected val dns = "(\\w|-)*".r

  protected def host : Parser[String] = (ip | dns)

  protected val ident : Parser[String] = """(\w|-)*""".r

  protected def prop  = rep1sep(ident, ".") ~ ("=" ~>  """(\w|[-\\/:])*""".r) ^^ { case p ~ v => (p.mkString("."), v)}

  def url : Parser[Properties] =
    ("jdbc:oce:dcom://" ~> host) ~ (":" ~> ident) ~ ("@" ~> ident) ~ (";" ~> repsep(prop, ";"))  ^^ {
    case host ~ user ~ pwd ~ props => {
      val r = new Properties
      r.setProperty("oce.host", host)
      r.setProperty("oce.host.user", user)
      r.setProperty("oce.host.password", pwd)
      props.foreach{
        prop => r.setProperty(prop._1, prop._2)
      }
      r
    }
  }

}