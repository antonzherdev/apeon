package ru.apeon.sync

import ru.apeon.core.loader.Listener
import ru.apeon.core.script.{ScriptDataTypeSeq, ScriptDataTypeEntity, ScriptDataTypeDescription}

class SyncListener extends Listener{
  def unload() {

  }

  def preLoad() {
    SyncListener.preLoad()
  }

  def load() {

  }
}

object SyncListener {
  def preLoad() {
    ScriptDataTypeDescription.addDeclaration(classOf[ScriptDataTypeEntity], SyncDeclaration)
    ScriptDataTypeDescription.addDeclaration(classOf[ScriptDataTypeSeq], SyncDeclaration)
  }
}