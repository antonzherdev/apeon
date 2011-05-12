package ru.apeon.core.eql

import ru.apeon.core.script

abstract class Function extends Expression{
  val name : String
}

case class SqlFunction(name : String, parameters : Seq[Expression], returnType : script.ScriptDataType) extends Function {
  override def children = parameters
  override def build(children: Seq[Expression]) = SqlFunction(name, children, returnType)
  def dataType(env: script.Environment) = returnType
}

abstract class AggFunction extends Function

case class Count() extends AggFunction {
  val name = "count"
  def dataType(env: script.Environment) = script.ScriptDataTypeInteger()
}

abstract class AggPar1Function extends AggFunction {
  def parameter : Expression

  override def children = Seq(parameter)

  def dataType(env: script.Environment) = parameter.dataType(env)

  override def build(children: Seq[Expression]) = build(children.head)

  def build(parameter : Expression) : Expression

  override def equals(obj: Any) = obj match {
    case e : AggPar1Function =>
      (e.name == this.name) &&
              (e.parameter == parameter)
    case _ => false
  }
}

case class Sum(parameter : Expression) extends AggPar1Function{
  val name = "sum"
  def build(parameter: Expression) = Sum(parameter)
}

case class Max(parameter : Expression) extends AggPar1Function{
  val name = "max"
  def build(parameter: Expression) = Max(parameter)
}

case class Min(parameter : Expression) extends AggPar1Function{
  val name = "min"
  def build(parameter: Expression) = Min(parameter)
}

case class Avg(parameter : Expression) extends AggPar1Function{
  val name = "avg"
  def build(parameter: Expression) = Avg(parameter)
}
