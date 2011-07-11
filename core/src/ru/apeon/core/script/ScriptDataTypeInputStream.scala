package ru.apeon.core.script

import java.io.{FileInputStream, File, InputStream}
import org.apache.commons.fileupload.util.Streams

case class ScriptDataTypeInputStream() extends ScriptDataType{
  override def valueOf = {
    case i : InputStream => i
    case f : File => new FileInputStream(f)
  }
}

object ScriptDataTypeInputStreamDescription {
  def declarations = Seq(toStringFunc)

  val toStringFunc = new Declaration {
    def name = "toString"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeString()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = Streams.asString(env.ref.asInstanceOf[InputStream])
  }
}