package ru.apeon.excel

import org.apache.poi.ss.usermodel.Sheet
import ru.apeon.core.script._
import org.apache.poi.ss.util.CellReference

case class ScriptDataTypeExcelSheet() extends ScriptDataType

object ScriptDataTypeExcelSheetDescription {
  def declarations : Seq[Declaration] = Seq(applyCell)

  val applyCell = new Declaration {
    def name = "apply"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeAny()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      val ref = new CellReference(parameters.get.head.value.asInstanceOf[String])
      Excel.toAny(env.ref.asInstanceOf[Sheet].getRow(ref.getRow).getCell(ref.getCol))
    }
    override def parameters = Seq(DefPar("ref", ScriptDataTypeString()))
  }
}