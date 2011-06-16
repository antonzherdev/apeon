package ru.apeon.core.script

/**
 * @author Anton Zherdev
 */

case class ScriptDataTypeTuple(dataTypes : Seq[ScriptDataType]) extends ScriptDataType {
  override lazy val declarations = {
    var i = -1
    dataTypes.map{dataType =>
      i += 1
      TupleValue(i, dataType)
    }
  } ++ Seq(HashCodeDeclaration)

  case class TupleValue(index : Int, valueDataType : ScriptDataType) extends Declaration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Seq[Any]].apply(index)

    val name = "_%d".format(index + 1)

    def dataType(env: Environment, parameters: Option[Seq[Par]]) = valueDataType
  }
}

case class Tuple(values : Seq[Expression]) extends Expression {
  def preFillRef(env: Environment, imports: Imports) {
    values.foreach{
      _.preFillRef(env, imports)
    }
  }

  def fillRef(env: Environment, imports: Imports) {
    values.foreach{
      _.fillRef(env, imports)
    }
  }

  def evaluate(env: Environment) = values.map(_.evaluate(env))

  def dataType(env: Environment) = ScriptDataTypeTuple(values.map(_.dataType(env)))
}