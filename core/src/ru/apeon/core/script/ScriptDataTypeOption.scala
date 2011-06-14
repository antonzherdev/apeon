package ru.apeon.core.script

case class ScriptDataTypeOption(dataType : ScriptDataType) extends ScriptDataType

object ScriptDataTypeOptionDescription {
  def declarations = Seq(get, isDefined, isEmpty, getOrElse)

  def tp(env : Environment) = env.dotType.get.asInstanceOf[ScriptDataTypeOption].dataType

  def get = new Declaration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.ref.asInstanceOf[Option[Any]].getOrElse(throw ScriptException("Get empty option object."))
    }
    def name = "get"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = tp(env)
  }

  def isDefined = new Declaration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.ref.asInstanceOf[Option[Any]].isDefined
    }
    def name = "isDefined"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeBoolean()
  }
  def isEmpty = new Declaration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.ref.asInstanceOf[Option[Any]].isEmpty
    }
    def name = "isEmpty"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeBoolean()
  }

  def getOrElse = new Declaration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.ref.asInstanceOf[Option[Any]].getOrElse{
        parameters.get.head.value.asInstanceOf[BuiltInFunction].run(env)
      }
    }
    def name = "getOrElse"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = tp(env)
    override def builtInParameters(env: Environment, parameters: Option[Seq[Par]], parameterNumber: Int, parameter: Par) = Seq()
    override def parameters = Seq(DefPar("default", ScriptDataTypeBuiltInFunction()))
  }
}