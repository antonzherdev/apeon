package ru.apeon.core.entity

import ru.apeon.core.sql.SqlDataType
import ru.apeon.core.script._

//TODO: Сделать перечисляемые типы ENUM
abstract class AttributeDataType {
  def toSqlDataType : SqlDataType
  def toScriptDataType : ScriptDataType
}

object AttributeDataType {
  def apply(nm : String, width : Option[Int], scale : Option[Int]) : AttributeDataType = nm match {
    case "Boolean" => AttributeDataTypeBoolean()
    case "String" => AttributeDataTypeString(width)
    case "Date" => AttributeDataTypeDate()
    case "Int" => AttributeDataTypeInteger()
    case "Dec" => (width, scale) match {
      case (Some(w), Some(s)) => AttributeDataTypeDecimal(w, s)
      case (Some(s), None) => AttributeDataTypeDecimal(30, s)
      case (None, None) => AttributeDataTypeDecimal(30, 17)
    }

    case _ => throw new RuntimeException("Unknown datatype \"%s\"".format(nm))
  }
}

case class AttributeDataTypeNull() extends AttributeDataType {
  def toSqlDataType = null
  def toScriptDataType = ScriptDataTypeNull()
}

case class AttributeDataTypeBoolean() extends AttributeDataType {
  def toSqlDataType = SqlDataType("bit")
  def toScriptDataType = ScriptDataTypeBoolean()
}
case class AttributeDataTypeString(width : Option[Int] = None) extends AttributeDataType {
  def toSqlDataType = width match {
    case Some(w) => SqlDataType("varchar", Some(w))
    case None => SqlDataType("text")
  }
  def toScriptDataType = ScriptDataTypeString()
}
case class AttributeDataTypeDate() extends AttributeDataType {
  def toSqlDataType = SqlDataType("datetime")
  def toScriptDataType = ScriptDataTypeDate()
}
case class AttributeDataTypeInteger() extends AttributeDataType {
  def toSqlDataType = SqlDataType("Int")
  def toScriptDataType = ScriptDataTypeInteger()
}
case class AttributeDataTypeDecimal(width : Int, scale : Int) extends AttributeDataType {
  def toSqlDataType = SqlDataType("Dec", Some(width), Some(scale))
  def toScriptDataType = ScriptDataTypeDecimal()
}