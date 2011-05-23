package ru.apeon.core.script

import ru.apeon.core._
import eql.SqlGeneration
import java.math.MathContext

case class ScriptDataTypeString() extends ScriptDataTypeSimple("string") {
  override def valueOf(str: String) = str
}

object ScriptDataTypeStringDescription {
  def declarations = Seq(format, toInt, toDec0, toDec1, replace)

  def format = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.dotRef.get.asInstanceOf[String].format(parameters.get.map{_.value} : _*)
    }
    def name = "format"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeString()
    override def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isDefined && (!parameters.get.isEmpty)
  }

  def toInt = new Declaration with SqlGeneration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.dotRef.get.asInstanceOf[String].toInt
    }
    def name = "toInt"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeInteger()
  def generateSql(ref: sql.Expression, parameters: Seq[sql.Expression]) = sql.Cast(ref, "int")
  }

  def toDec0 = new Declaration with SqlGeneration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      BigDecimal(env.dotRef.get.asInstanceOf[String], MathContext.DECIMAL128)
    }
    def name = "toDec"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeDecimal()
    def generateSql(ref: sql.Expression, parameters: Seq[sql.Expression]) = sql.Cast(ref, "dec")
  }

  def toDec1 = new Declaration with SqlGeneration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      BigDecimal(env.dotRef.get.asInstanceOf[String], MathContext.DECIMAL128).setScale(
        parameters.get.head.value.asInstanceOf[Int], BigDecimal.RoundingMode.FLOOR)
    }
    def name = "toDec"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeDecimal()
    override def parameters = Seq(DefPar("scale", ScriptDataTypeInteger()))
    def generateSql(ref: sql.Expression, parameters: Seq[sql.Expression]) =
      sql.Cast(ref, "dec(30, %s)".format(parameters.head))
  }

  def replace = new Declaration with SqlGeneration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      val pars = parameters.get
      env.dotRef.get.asInstanceOf[String].replace(pars(0).value.asInstanceOf[String], pars(1).value.asInstanceOf[String])
    }
    def name = "replace"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeString()
    override def parameters = Seq(DefPar("target", ScriptDataTypeString()), DefPar("replacement", ScriptDataTypeString()))
    def generateSql(ref: sql.Expression, parameters: Seq[sql.Expression]) =
      sql.Call("replace", Seq(ref, parameters(0), parameters(1)))
  }
}