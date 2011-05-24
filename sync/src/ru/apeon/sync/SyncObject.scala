package ru.apeon.sync

import ru.apeon.core.script._

object SyncObject extends ObjectBase {
  val InsertOnly = 1
  val NoAutoUpdate = 2
  val NoAutoUpdateToOne = 4
  val NoAutoUpdateToMany = 8
  val ToManyAppend = 16

  def module = CoreModule
  def pack = EmptyPackage

  def name = "Sync"
  def extendsClass = None
  def declaredDeclarations = Seq(
    ConstDeclaration("InsertOnly", InsertOnly),
    ConstDeclaration("NoAutoUpdate", NoAutoUpdate),
    ConstDeclaration("NoAutoUpdateToOne", NoAutoUpdateToOne),
    ConstDeclaration("NoAutoUpdateToMany", NoAutoUpdateToMany),
    ConstDeclaration("ToManyAppend", ToManyAppend)
  )

  case class ConstDeclaration(name : String, value : Int) extends DeclarationStatement {
    def dataType(env: Environment) = ScriptDataTypeInteger()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = this.value
  }
}