package ru.apeon.core.script


abstract class ScriptDataTypeIterable extends ScriptDataType{
  def dataType : ScriptDataType
}

case class ScriptDataTypeSeq(dataType : ScriptDataType) extends ScriptDataTypeIterable {

}
case class ScriptDataTypeMap(keyDataType : ScriptDataType, valueDataType : ScriptDataType) extends ScriptDataTypeIterable {
  val dataType = ScriptDataTypeKeyValue(Map("key" -> keyDataType, "value" -> valueDataType))
}

object ScriptDataTypeSeqDescription {
  def iterable = Seq(foreach, filter, filterNot, find, isEmpty, size, groupBy)
  def map = iterable
  def seq = iterable

  def t(env : Environment) = env.dotType.get.asInstanceOf[ScriptDataTypeSeq]
  def tp(env : Environment) = env.dotType.get.asInstanceOf[ScriptDataTypeSeq].dataType

  abstract class OneBuiltInDeclaration extends Declaration {
    override def builtInParameters(env: Environment, parameters: Option[Seq[Par]], parameterNumber: Int, parameter: Par) =
      Seq(BuiltInParameterDef("_", tp(env)))
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      value(env,  env.ref.asInstanceOf[Iterable[Any]], parameters.get.head.value.asInstanceOf[BuiltInFunction])
    }
    def value(env : Environment, items : Iterable[Any], f : BuiltInFunction) : Any
    override def parameters = Seq(DefPar("f", ScriptDataTypeBuiltInFunction()))
  }

  val foreach = new OneBuiltInDeclaration{
    def name = "foreach"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeUnit()
    def value(env: Environment, items: Iterable[Any], f: BuiltInFunction) {
      items.foreach {
        item => f.run(env, item)
      }
    }
  }

  val filter = new OneBuiltInDeclaration{
    def name = "filter"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = t(env)
    def value(env: Environment, items: Iterable[Any], f: BuiltInFunction) = items.filter{item =>
      f.run(env, item).asInstanceOf[Boolean]}
  }

  val filterNot = new OneBuiltInDeclaration{
    def name = "filterNot"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = t(env)
    def value(env: Environment, items: Iterable[Any], f: BuiltInFunction) = items.filterNot{item =>
      f.run(env, item).asInstanceOf[Boolean]}
  }


  val find = new OneBuiltInDeclaration{
    def name = "find"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) =
      ScriptDataTypeOption(tp(env))
    def value(env: Environment, items: Iterable[Any], f: BuiltInFunction) = items.find {
      item => f.run(env, item).asInstanceOf[Boolean]
    }
  }

  val isEmpty = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Seq[_]].isEmpty
    def name = "isEmpty"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeBoolean()
  }

  val size = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Seq[_]].size
    def name = "size"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeInteger()
  }

  val groupBy = new OneBuiltInDeclaration {
    def name = "groupBy"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) =
      ScriptDataTypeMap(parameters.get.head.expression.evaluate(env).asInstanceOf[BuiltInFunction].statement.dataType(env), t(env))
    def value(env: Environment, items: Iterable[Any], f: BuiltInFunction) =
      items.groupBy(item => f.run(env, item))
  }
}
