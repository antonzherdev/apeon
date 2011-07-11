package ru.apeon.sync.buffer

import java.io.InputStream

trait BufferItem {
  def name : String
  def stream : InputStream

  def end()
}

trait BufferImpl {
  def init(url : String)

  def items : Seq[BufferItem]
}