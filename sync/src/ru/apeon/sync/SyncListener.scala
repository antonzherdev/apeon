package ru.apeon.sync

import ru.apeon.core.loader.Listener
import ru.apeon.core.script.{ObjectModel, ScriptDataTypeSeq, ScriptDataTypeEntity, ScriptDataTypeDescription}

class SyncListener extends Listener{
  def unload(model : ObjectModel) {

  }

  def preLoad(model : ObjectModel) {
    SyncListener.preLoad(model)
  }

  def load(model : ObjectModel) {

  }
}

object SyncListener {
  def preLoad(model : ObjectModel) {
    ScriptDataTypeDescription.addDeclaration(classOf[ScriptDataTypeEntity], SyncDeclaration)
    ScriptDataTypeDescription.addDeclaration(classOf[ScriptDataTypeEntity], SyncFindDeclaration)
    ScriptDataTypeDescription.addDeclaration(classOf[ScriptDataTypeSeq], SyncDeclaration)
    model.addObj(SyncObject)
    model.addObj(SkipObject)
  }
}