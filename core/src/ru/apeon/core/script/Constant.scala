package ru.apeon.core.script

trait Constant extends Expression {
  def value : Any

  def evaluate(env: Environment) = value

  def fillRef(env: Environment, imports: Imports) {}

  def preFillRef(model: ObjectModel, imports: Imports) {}
}

case class ConstInt(value : Int) extends Constant {
  def dataType(env: Environment) = ScriptDataTypeInteger()
}

case class ConstDecimal(value : BigDecimal) extends Constant {
  def dataType(env: Environment) = ScriptDataTypeDecimal()
}

case class ConstString(value : String) extends Constant {
  def dataType(env: Environment) = ScriptDataTypeString()
}

case class ConstSeq(expressions : Seq[Expression]) extends Expression{
  def evaluate(env: Environment) = expressions.map(_.evaluate(env))

  def dataType(env: Environment) = ScriptDataTypeSeq(ScriptDataTypeAny())

  def fillRef(env: Environment, imports: Imports) {
    expressions.foreach(_.fillRef(env, imports))
  }

  def preFillRef(model: ObjectModel, imports: Imports) {
    expressions.foreach(_.preFillRef(model, imports))
  }
}

case class ConstNull() extends Constant {
  def dataType(env: Environment) = ScriptDataTypeNull()

  def value = null
}
