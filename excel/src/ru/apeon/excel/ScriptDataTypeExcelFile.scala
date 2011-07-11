package ru.apeon.excel

import org.apache.poi.ss.usermodel.Workbook
import ru.apeon.core.script._

case class ScriptDataTypeExcelFile() extends ScriptDataType

object ScriptDataTypeExcelFileDescription {
  def declarations : Seq[Declaration] = Seq(ApplyName("apply"), ApplyName("sheet"), ApplyNumber("apply"), ApplyNumber("sheet"))

  case class ApplyName(name : String) extends Declaration{
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeExcelSheet()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Workbook].getSheet(parameters.get.head.value.asInstanceOf[String])
    override def parameters = Seq(DefPar("name", ScriptDataTypeString()))
  }
  case class ApplyNumber(name : String) extends Declaration {
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeExcelSheet()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Workbook].getSheetAt(parameters.get.head.value.asInstanceOf[Int])
    override def parameters = Seq(DefPar("name", ScriptDataTypeInteger()))
  }
}