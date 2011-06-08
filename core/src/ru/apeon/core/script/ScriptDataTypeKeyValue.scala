package ru.apeon.core.script

case class ScriptDataTypeKeyValue(keys : Map[String, ScriptDataType]) extends ScriptDataType {
  val _declarations = keys.map(k => KeyDeclaration(k._1, k._2)).toSeq

  case class KeyDeclaration(name : String, dataType : ScriptDataType) extends Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[collection.Map[String, Any]].apply(name)
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = dataType
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
  }

  override def declarations = _declarations
}