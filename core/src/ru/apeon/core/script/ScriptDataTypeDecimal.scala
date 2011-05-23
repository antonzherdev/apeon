package ru.apeon.core.script

import java.math.{MathContext}

case class ScriptDataTypeDecimal() extends ScriptDataTypeSimple("dec") {
  override def valueOf(str: String) = BigDecimal(str)
}

object ScriptDataTypeDecimalDescription {
  def declarations = Seq(roundPar, round)

  def roundPar = new Declaration {
    def name = "round"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeDecimal()
    override def parameters = Seq(DefPar("scale", ScriptDataTypeInteger()))
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[BigDecimal].setScale(parameters.get.head.value.asInstanceOf[Int], BigDecimal.RoundingMode.HALF_UP)
  }
  def round = new Declaration {
    def name = "round"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeInteger()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[BigDecimal].round(new MathContext(1)).toInt
  }
}