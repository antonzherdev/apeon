package ru.apeon.sync

import ru.apeon.core.script._

/**
 * @author Anton Zherdev
 */

object SkipObject extends ObjectBase {
  def name = "syncSkip"
  def pack = EmptyPackage
  def module = CoreModule
  def extendsClass = None

  def declaredDeclarations = Seq(apply)

  val apply = new DeclarationStatement {
    def name = "apply"
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) {}
    override def parameters = Seq(DefPar("name", ScriptDataTypeString()))
    def dataType(env: Environment) = ScriptDataTypeUnit()
  }
}