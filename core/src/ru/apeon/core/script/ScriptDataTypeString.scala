package ru.apeon.core.script

import ru.apeon.core._
import eql.SqlGeneration


/**
 * @author Anton Zherdev
 */

case class ScriptDataTypeString() extends ScriptDataTypeSimple("string") {
  override def declarations = Seq(format, toInt)

  def format = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.dotRef.get.asInstanceOf[String].format(parameters.get.map{_.value} : _*)
    }
    def name = "format"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeString()
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isDefined && (!parameters.get.isEmpty)
  }

  def toInt = new Declaration with SqlGeneration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.dotRef.get.asInstanceOf[String].toInt
    }
    def name = "toInt"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeInteger()
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty

    def generateSql(ref: sql.Expression, parameters: Seq[sql.Expression]) = sql.Cast(ref, "int")
  }
}