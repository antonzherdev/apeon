package ru.apeon.core.script

import java.math.{MathContext}

case class ScriptDataTypeDecimal() extends ScriptDataTypeSimple("dec") {
  override def declarations = Seq(roundPar, round)

  def roundPar = new Declaration {
    def name = "round"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeDecimal()
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters match {
      case Some(Seq(par)) => par.expression.dataType(env) == ScriptDataTypeInteger()
      case _ => false
    }
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[BigDecimal].setScale(parameters.get.head.value.asInstanceOf[Int], BigDecimal.RoundingMode.HALF_UP)
  }
  def round = new Declaration {
    def name = "round"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeInteger()
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty

    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[BigDecimal].round(new MathContext(1)).toInt
  }

  override def valueOf(str: String) = BigDecimal(str)
}