package ru.apeon.sync.buffer

import java.io.{FileInputStream, File}

/**
 * @author Anton Zherdev
 */

class FileBuffer extends BufferImpl{
  var dir : File = _

  def items = {
    dir.listFiles().toSeq.map {file =>
      FileBufferItem(file)
    }
  }

  def init(url: String) {
    dir = new File(url)
    if(!dir.exists()) {
      dir.mkdirs()
    }
  }

  case class FileBufferItem(file : File) extends BufferItem{
    def stream = new FileInputStream(file)

    def name = file.getName

    def end() {
      file.delete()
    }
  }
}