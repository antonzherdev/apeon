package ru.apeon.core.script

import akka.util.Logging


object LogObject extends ObjectBase {
  def module = CoreModule
  def pack = EmptyPackage

  def name = "Log"
  def extendsClass = None
  def declaredDeclarations = Seq(
    new WriteDeclaration{
      def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) {
        log.info(parameters.get.head.value.toString)
      }
      def name = "info"
    },
    new WriteDeclaration{
      def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) {
        log.debug(parameters.get.head.value.toString)
      }
      def name = "debug"
    },
    new WriteDeclaration{
      def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) {
        log.error(parameters.get.head.value.toString)
      }
      def name = "error"
    },
    new WriteDeclaration{
      def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) {
        log.warn(parameters.get.head.value.toString)
      }
      def name = "warning"
    }
  )

  abstract class WriteDeclaration extends DeclarationStatement with Logging {
    def dataType(env: Environment) = ScriptDataTypeUnit()
    override def parameters = Seq(DefPar("text", ScriptDataTypeString()))
  }
}