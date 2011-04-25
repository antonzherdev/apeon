package ru.apeon.core.script

/**
 * @author Anton Zherdev
 */

case class ScriptDataTypeString() extends ScriptDataTypeSimple("string") {
  override def declarations = Seq(format)

  def format = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.dotRef.get.asInstanceOf[String].format(parameters.get.map{_.value} : _*)
    }
    def name = "format"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeString()
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isDefined && (!parameters.get.isEmpty)
  }
}