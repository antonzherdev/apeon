package ru.apeon.sync.buffer

import ru.apeon.core.script._

case class Buffer(module : Module, pack : Package, name : String, declaredDeclarations : Seq[DeclarationStatement])
        extends ObjectBase
{
  val extendsClass = None

  lazy val applyDef = declarations.find{
      case dec : Def =>
        dec.name == "apply" && dec.parameters.size == 2 && dec.parameters(0).dataType == ScriptDataTypeString() &&
        dec.parameters(1).dataType == ScriptDataTypeInputStream()
      case _ => false
    }.get.asInstanceOf[Def]
}