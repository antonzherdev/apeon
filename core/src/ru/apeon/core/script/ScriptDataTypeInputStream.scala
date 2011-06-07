package ru.apeon.core.script

import java.io.{FileInputStream, File, InputStream}

case class ScriptDataTypeInputStream() extends ScriptDataType{
  override def valueOf = {
    case i : InputStream => i
    case f : File => new FileInputStream(f)
  }
}