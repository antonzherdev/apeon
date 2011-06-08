package ru.apeon.core.entity

import ru.apeon.core.sql.SqlDataType
import ru.apeon.core.script._

//TODO: Сделать перечисляемые типы ENUM
abstract class AttributeDataType {
  def toSqlDataType : SqlDataType
  def toScriptDataType : ScriptDataType
}

object AttributeDataType {
  def apply(nm : String, width : Int, scale : Int) : AttributeDataType = nm match {
    case "boolean" => AttributeDataTypeBoolean()
    case "char" => AttributeDataTypeChar(width)
    case "date" => AttributeDataTypeDate()
    case "datetime" => AttributeDataTypeDateTime()
    case "integer" => AttributeDataTypeInteger()
    case "decimal" => AttributeDataTypeDecimal(width, scale)
    case "text" => AttributeDataTypeText()
    case "time" => AttributeDataTypeTime()
    case "varchar" => AttributeDataTypeVarchar(width)
    case "uid" => AttributeDataTypeUID()
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
case class AttributeDataTypeChar(width : Int) extends AttributeDataType {
  def toSqlDataType = SqlDataType("char", Some(width))
  def toScriptDataType = ScriptDataTypeString()
}
case class AttributeDataTypeDate() extends AttributeDataType {
  def toSqlDataType = SqlDataType("date")
  def toScriptDataType = ScriptDataTypeDate()
}
case class AttributeDataTypeDateTime() extends AttributeDataType {
  def toSqlDataType = SqlDataType("datetime")
  def toScriptDataType = ScriptDataTypeDate()
}
case class AttributeDataTypeInteger() extends AttributeDataType {
  def toSqlDataType = SqlDataType("integer")
  def toScriptDataType = ScriptDataTypeInteger()
}
case class AttributeDataTypeDecimal(width : Int, scale : Int) extends AttributeDataType {
  def toSqlDataType = SqlDataType("decimal", Some(width), Some(scale))
  def toScriptDataType = ScriptDataTypeDecimal()
}
case class AttributeDataTypeText() extends AttributeDataType {
  def toSqlDataType = SqlDataType("text")
  def toScriptDataType = ScriptDataTypeString()
}
case class AttributeDataTypeTime() extends AttributeDataType {
  def toSqlDataType = SqlDataType("time")
  def toScriptDataType = ScriptDataTypeDate()
}
case class AttributeDataTypeVarchar(width : Int) extends AttributeDataType {
  def toSqlDataType = SqlDataType("varchar", Some(width))
  def toScriptDataType = ScriptDataTypeString()
}

case class AttributeDataTypeUID() extends AttributeDataType {
  def toSqlDataType = SqlDataType("uniqueidentifier")
  def toScriptDataType = ScriptDataTypeString()
}