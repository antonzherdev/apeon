package ru.apeon.core.script

trait Constant extends Expression {
  def value : Any

  def evaluate(env: Environment) = value

  def fillRef(env: Environment, imports: Imports) {}

  def preFillRef(env : Environment, imports: Imports) {}

  override def toString = value.toString
}

case class ConstInt(value : Int) extends Constant {
  def dataType(env: Environment) = ScriptDataTypeInteger()
}

case class ConstString(value : String) extends Constant {
  def dataType(env: Environment) = ScriptDataTypeString()
}

case class ConstSeq(expressions : Seq[Expression]) extends Expression{
  def evaluate(env: Environment) = expressions.map(_.evaluate(env))

  def dataType(env: Environment) = ScriptDataTypeSeq(ScriptDataTypeAny())

  def fillRef(env: Environment, imports: Imports) {
    expressions.foreach(exp => env.fillRef(exp, imports))
  }

  def preFillRef(env : Environment, imports: Imports) {
    expressions.foreach(env.preFillRef(_, imports))
  }
}

case class ConstNull() extends Constant {
  def dataType(env: Environment) = ScriptDataTypeNull()

  def value = null
}
