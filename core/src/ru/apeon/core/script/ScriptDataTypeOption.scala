package ru.apeon.core.script

case class ScriptDataTypeOption(dataType : ScriptDataType) extends ScriptDataType {
  override def declarations = Seq(get, isDefined, isEmpty, getOrElse)

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

  def getOrElse = new Declaration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.ref.asInstanceOf[Option[Any]].getOrElse{
        parameters.get.head.value.asInstanceOf[BuiltInFunction].run(env)
      }
    }
    def name = "getOrElse"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeOption.this.dataType
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters match {
      case Some(Seq(Par(b : BuiltInFunction, _))) => true
      case _ => false
    }
    override def builtInParametersDataTypes(env: Environment, parameterNumber: Int, parameter: Par) = Seq()
  }
}