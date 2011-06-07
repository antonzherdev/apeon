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

  def valueOf : PartialFunction[Any, Any] = new PartialFunction[Any, Any] {
    def isDefinedAt(x: Any) = false
    def apply(v1: Any) = throw new MatchError
  }

  def correspond(dataType : ScriptDataType) : Boolean = dataType == this
}

object ScriptDataTypeDescription {
  private val _declarations = mutable.Map.empty[Class[_], Buffer[Declaration]]

  def declarations(clazz : Class[_]) = _declarations.getOrElse(clazz, Seq())

  def addDeclaration(clazz : Class[_], declarations : Declaration*) {
    _declarations.getOrElseUpdate(clazz, Buffer.empty[Declaration]).append(declarations : _*)
  }

  def load() {
    _declarations.clear()
    addDeclaration(classOf[ScriptDataTypeDate], ScriptDataTypeDateDescription.declarations : _*)
    addDeclaration(classOf[ScriptDataTypeDecimal], ScriptDataTypeDecimalDescription.declarations : _*)
    addDeclaration(classOf[ScriptDataTypeEntity], ScriptDataTypeEntityTypeDescription.declarations : _*)
    addDeclaration(classOf[ScriptDataTypeEntityDescription], ScriptDataTypeEntityDescriptionTypeDescription.declarations : _*)
    addDeclaration(classOf[ScriptDataTypeOption], ScriptDataTypeOptionDescription.declarations : _*)

    addDeclaration(classOf[ScriptDataTypeSeq], ScriptDataTypeSeqDescription.seq : _*)
    addDeclaration(classOf[ScriptDataTypeMap], ScriptDataTypeSeqDescription.map : _*)

    addDeclaration(classOf[ScriptDataTypeString], ScriptDataTypeStringDescription.declarations : _*)
    addDeclaration(classOf[ScriptDataTypeEqlSelectBase], ScriptDataTypeEqlSelectBaseDescription.declarations : _*)
    addDeclaration(classOf[ScriptDataTypeEqlStatement], ScriptDataTypeEqlStatementDescription.declarations : _*)

    addDeclaration(classOf[ScriptDataTypeInteger], ToStringDeclaration)
    addDeclaration(classOf[ScriptDataTypeAny], ToStringDeclaration)
    addDeclaration(classOf[ScriptDataTypeBoolean], ToStringDeclaration)
  }
  load()
}

case class ScriptDataTypeDataSource() extends ScriptDataType

case class ScriptDataTypeAny() extends ScriptDataType {
  override def valueOf = {case v => v}
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
  override def valueOf = {
    case v : String => v.toInt
    case i : Int => i
  }
}

case class ScriptDataTypeBuiltInFunction() extends ScriptDataType

object ToStringDeclaration extends Declaration {
  def name = "toString"
  def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeString()
  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = env.ref.toString
}