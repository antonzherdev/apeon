package ru.apeon.core.script

import java.math.{RoundingMode, MathContext}

abstract class ScriptDataType {
  def declarations : Seq[Declaration] = Seq()

  def declaration(env: Environment, name : String, parameters : Option[Seq[Par]] = None) = {
    declarations.find(declaration => declaration.name == name && declaration.correspond(env, parameters))
  }

  def preFillRef(model : ObjectModel, imports : Imports) {}
  def fillRef(env: Environment, imports: Imports) {}

  def valueOf(str : String) : Any = throw new ScriptException("Unsupported conversation from string to \"%s\".".format(getClass))
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