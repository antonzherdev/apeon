package ru.apeon.excel

import ru.apeon.core.script._
import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.apache.poi.ss.usermodel.{DateUtil, Cell}
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.apache.commons.fileupload.util.Streams
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, FileInputStream}

object Excel extends ObjectBase{
  def name = "Excel"
  def pack = EmptyPackage
  def module = CoreModule
  def extendsClass = None

  def declaredDeclarations = Seq(apply, applyStream)

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

  val applyStream = new DeclarationStatement {
    def name = "apply"

    def dataType(env: Environment) = ScriptDataTypeExcelFile()

    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      val stream = parameters.get.head.value.asInstanceOf[InputStream]
      val baos = new ByteArrayOutputStream
      Streams.copy(stream, baos, true)
      val bites = baos.toByteArray
      try {
        new HSSFWorkbook(new ByteArrayInputStream(bites))
      }
      catch {
        case _ => new XSSFWorkbook(new ByteArrayInputStream(bites))
      }
    }

    override def parameters = Seq(DefPar("stream", ScriptDataTypeInputStream()))
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
    case Cell.CELL_TYPE_BLANK =>
      null
  }
}