package ru.apeon.core.script

import java.math.{RoundingMode, MathContext}

abstract class ScriptDataType {
  def declarations : Seq[Declaration] = Seq()

  def declaration(env: Environment, name : String, parameters : Option[Seq[Par]] = None) = {
    declarations.find(declaration => declaration.name == name && declaration.correspond(env, parameters))
  }

  def preFillRef(model : ObjectModel, imports : Imports) {}
  def fillRef(env: Environment, imports: Imports) {}
}

case class ScriptDataTypeDataSource() extends ScriptDataType

case class ScriptDataTypeAny() extends ScriptDataType
case class ScriptDataTypeUnit() extends ScriptDataType
case class ScriptDataTypeNull() extends ScriptDataType
case class ScriptDataTypeSync() extends ScriptDataType
case class ScriptDataTypePackage(pack : Package) extends ScriptDataType {
  override def declaration(env: Environment, name: String, parameters: Option[Seq[Par]]) =
    env.globalDeclarationOption(name, parameters, Some(Imports(pack)))
}

abstract class ScriptDataTypeSimple(val name : String) extends ScriptDataType
case class ScriptDataTypeBoolean() extends ScriptDataTypeSimple("boolean")
case class ScriptDataTypeInteger() extends ScriptDataTypeSimple("int")

case class ScriptDataTypeBuiltInFunction() extends ScriptDataType