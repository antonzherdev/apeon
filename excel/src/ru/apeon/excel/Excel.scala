package ru.apeon.excel

import ru.apeon.core.script._
import org.apache.poi.hssf.usermodel.HSSFWorkbook
import java.io.FileInputStream
import org.apache.poi.ss.usermodel.{DateUtil, Cell}
import org.apache.poi.xssf.usermodel.XSSFWorkbook

object Excel extends ObjectBase{
  def name = "Excel"
  def pack = EmptyPackage
  def module = CoreModule
  def extendsClass = None

  def declaredDeclarations = Seq(apply)

  val apply = new DeclarationStatement {
    def name = "apply"

    def dataType(env: Environment) = ScriptDataTypeExcelFile()

    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      val fileName = parameters.get.head.value.asInstanceOf[String]
      if(fileName.endsWith(".xlsx") ) {
        new XSSFWorkbook(new FileInputStream(fileName))
      } else {
        new HSSFWorkbook(new FileInputStream(fileName))
      }
    }

    override def parameters = Seq(DefPar("fileName", ScriptDataTypeString()))
  }

  def toAny(cell : Cell) : Any = cell.getCellType match {
    case Cell.CELL_TYPE_STRING => cell.getRichStringCellValue.getString
    case Cell.CELL_TYPE_NUMERIC =>
      if(DateUtil.isCellDateFormatted(cell)) {
        cell.getDateCellValue
      } else {
        cell.getNumericCellValue
      }
    case Cell.CELL_TYPE_BOOLEAN =>
      cell.getBooleanCellValue
  }
}