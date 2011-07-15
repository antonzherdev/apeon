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
    },
    new DeclarationStatement {
      def name = "apply"
      def dataType(env: Environment) = ScriptDataTypeDate()
      def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
        val cal = Calendar.getInstance
        val p = parameters.get.map{_.value.asInstanceOf[Int]}
        cal.clear()
        cal.set(p(0), p(1) - 1, p(2), 0, 0, 0)
        cal.getTime
      }
      override def parameters = Seq(DefPar("year", ScriptDataTypeInteger()),
        DefPar("month", ScriptDataTypeInteger()), DefPar("day", ScriptDataTypeInteger()))
    } ,
    new DeclarationStatement {
      def name = "apply"
      def dataType(env: Environment) = ScriptDataTypeDate()
      def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
        val cal = Calendar.getInstance
        val p = parameters.get.map{_.value.asInstanceOf[Int]}
        cal.clear()
        cal.set(p(0), p(1) - 1, p(2), p(3), p(4), p(5))
        cal.getTime
      }
      override def parameters = Seq(DefPar("year", ScriptDataTypeInteger()),
        DefPar("month", ScriptDataTypeInteger()), DefPar("day", ScriptDataTypeInteger()),
        DefPar("hour", ScriptDataTypeInteger()), DefPar("minute", ScriptDataTypeInteger()),
        DefPar("second", ScriptDataTypeInteger()))
    }
  )
}