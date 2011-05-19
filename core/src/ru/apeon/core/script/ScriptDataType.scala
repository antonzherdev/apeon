package ru.apeon.core.script

import collection.mutable
import mutable.Buffer

abstract class ScriptDataType {
  lazy val declarations : Seq[Declaration] = ScriptDataTypeDescription.declarations(this.getClass)

  def declaration(env: Environment, name : String, parameters : Option[Seq[Par]] = None) = {
    declarations.find(declaration => declaration.name == name && declaration.correspond(env, parameters)).map{ d=>
      DeclarationThis(Some(this), d)
    }
  }

  def preFillRef(env : Environment, imports : Imports) {}
  def fillRef(env: Environment, imports: Imports) {}

  def valueOf(str : String) : Any = throw new ScriptException("Unsupported conversation from string to \"%s\".".format(getClass))
}

object ScriptDataTypeDescription {
  private val _declarations = mutable.Map.empty[Class[_], Buffer[Declaration]]

  def declarations(clazz : Class[_]) = _declarations.getOrElse(clazz, Seq())

  def addDeclaration(clazz : Class[_], declarations : Declaration*) {
    _declarations.getOrElseUpdate(clazz, Buffer.empty[Declaration]).append(declarations : _*)
  }

  addDeclaration(classOf[ScriptDataTypeDate], ScriptDataTypeDateDescription.declarations : _*)
  addDeclaration(classOf[ScriptDataTypeDecimal], ScriptDataTypeDecimalDescription.declarations : _*)
  addDeclaration(classOf[ScriptDataTypeEntity], ScriptDataTypeEntityTypeDescription.declarations : _*)
  addDeclaration(classOf[ScriptDataTypeEntityDescription], ScriptDataTypeEntityDescriptionTypeDescription.declarations : _*)
  addDeclaration(classOf[ScriptDataTypeOption], ScriptDataTypeOptionDescription.declarations : _*)
  addDeclaration(classOf[ScriptDataTypeSeq], ScriptDataTypeSeqDescription.declarations : _*)
  addDeclaration(classOf[ScriptDataTypeString], ScriptDataTypeStringDescription.declarations : _*)
  addDeclaration(classOf[ScriptDataTypeEqlSelectBase], ScriptDataTypeEqlSelectBaseDescription.declarations : _*)
  addDeclaration(classOf[ScriptDataTypeEqlStatement], ScriptDataTypeEqlStatementDescription.declarations : _*)
}

case class ScriptDataTypeDataSource() extends ScriptDataType

case class ScriptDataTypeAny() extends ScriptDataType {
  override def valueOf(str: String) = str
}
case class ScriptDataTypeUnit() extends ScriptDataType
case class ScriptDataTypeNull() extends ScriptDataType
case class ScriptDataTypeSync() extends ScriptDataType
case class ScriptDataTypePackage(pack : Package) extends ScriptDataType {
  override def declaration(env: Environment, name: String, parameters: Option[Seq[Par]]) =
    env.globalDeclarationOption(name, parameters, Some(Imports(pack)))
}

abstract class ScriptDataTypeSimple(val name : String) extends ScriptDataType
case class ScriptDataTypeBoolean() extends ScriptDataTypeSimple("boolean")
case class ScriptDataTypeInteger() extends ScriptDataTypeSimple("int") {
  override def valueOf(str: String) = str.toInt
}

case class ScriptDataTypeBuiltInFunction() extends ScriptDataType