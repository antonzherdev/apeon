package ru.apeon.core.script

import java.util.Calendar


object DateObject extends ObjectBase {
  def module = CoreModule
  def pack = EmptyPackage

  def name = "Date"
  def extendsClass = None
  def declaredDeclarations = Seq(
    new DeclarationStatement{
      def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
        Calendar.getInstance.getTime
      }
      def name = "now"
      def dataType(env: Environment) = ScriptDataTypeDate()
      def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
    }
  )
}