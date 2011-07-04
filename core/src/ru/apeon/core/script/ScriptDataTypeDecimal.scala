package ru.apeon.core.script

import java.math.{MathContext}

case class ScriptDataTypeDecimal() extends ScriptDataTypeSimple("dec") {
  override def valueOf = {
    case b : BigDecimal => b
    case s : String => BigDecimal(s)
  }
}

object ScriptDataTypeDecimalDescription {
  def declarations = Seq(roundPar, round, HashCodeDeclaration)

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

  val between = new BetweenDeclaration[BigDecimal] {
    def dataType = ScriptDataTypeDecimal()
    def compare(min: BigDecimal, max: BigDecimal) = min <= max
  }
}