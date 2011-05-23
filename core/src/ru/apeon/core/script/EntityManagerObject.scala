package ru.apeon.core.script

object EntityManagerObject extends ObjectBase {
  def module = CoreModule
  def pack = EmptyPackage

  def name = "EntityManager"
  def extendsClass = None
  def declaredDeclarations = Seq(new DeclarationStatement{
    def dataType(env: Environment) = ScriptDataTypeUnit()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) {
      env.em.transaction{}
    }
    def name = "commit"
  }
  )
}