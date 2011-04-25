package ru.apeon.core.script

case class ScriptDataTypeOption(dataType : ScriptDataType) extends ScriptDataType {
  override def declarations = Seq(get, isDefined, isEmpty)

  def get = new Declaration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.ref.asInstanceOf[Option[Any]].getOrElse(throw ScriptException(env, "Get empty option object."))
    }
    def name = "get"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeOption.this.dataType
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
  }

  def isDefined = new Declaration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.ref.asInstanceOf[Option[Any]].isDefined
    }
    def name = "isDefined"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeBoolean()
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
  }
  def isEmpty = new Declaration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.ref.asInstanceOf[Option[Any]].isEmpty
    }
    def name = "isEmpty"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeBoolean()
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
  }
}