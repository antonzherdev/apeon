package ru.apeon.excel

import ru.apeon.core.loader.Listener
import ru.apeon.core.script.{ScriptDataTypeDescription, ObjectModel}

class ExcelListener extends Listener{
  def unload(model: ObjectModel) {
  }

  def preLoad(model: ObjectModel) {
    model.addObj(Excel)
    ScriptDataTypeDescription.addDeclaration(classOf[ScriptDataTypeExcelFile], ScriptDataTypeExcelFileDescription.declarations : _*)
    ScriptDataTypeDescription.addDeclaration(classOf[ScriptDataTypeExcelSheet], ScriptDataTypeExcelSheetDescription.declarations : _*)
    ScriptDataTypeDescription.addDeclaration(classOf[ScriptDataTypeExcelRow], ScriptDataTypeExcelRowDescription.declarations : _*)
  }

  def load(model: ObjectModel) {

  }
}