package ru.apeon.core.script

/**
 * @author Anton Zherdev
 */

case class EntityManagerObject(pack : Package) extends ObjectBase {
  def name = "EntityManager"
  def extendsClass = None
  def declaredDeclarations = Seq(new DeclarationStatement{
    def dataType(env: Environment) = ScriptDataTypeUnit()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) {
      env.em.transaction{}
    }
    def name = "commit"
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
  }
  )
}