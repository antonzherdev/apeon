package ru.apeon.excel

import ru.apeon.core.script._
import org.apache.poi.ss.util.CellReference
import org.apache.poi.ss.usermodel.{Row}

case class ScriptDataTypeExcelRow() extends ScriptDataType

object ScriptDataTypeExcelRowDescription {
  def declarations : Seq[Declaration] = Seq(applyCell, number)

  val applyCell = new Declaration {
    def name = "apply"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeAny()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      val row = env.ref.asInstanceOf[Row]
      val ref = new CellReference(parameters.get.head.value.asInstanceOf[String] + row.getRowNum.toString
      )
      Excel.toAny(row.getCell(ref.getCol))
    }
    override def parameters = Seq(DefPar("name", ScriptDataTypeString()))
  }

  val number = new Declaration {
    def name = "number"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeInteger()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Row].getRowNum + 1
  }
}
