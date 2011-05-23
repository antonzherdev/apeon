package ru.apeon.core.script

case class ScriptDataTypeSeq(dataType : ScriptDataType) extends ScriptDataType

object ScriptDataTypeSeqDescription {
  def declarations = Seq(foreach, filter, filterNot, find, isEmpty, size)

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

  def foreach = new OneBuiltInDeclaration{
    def name = "foreach"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeUnit()
    def value(env: Environment, items: Iterable[Any], f: BuiltInFunction) {
      items.foreach {
        item => f.run(env, item)
      }
    }
  }

  def filter = new OneBuiltInDeclaration{
    def name = "filter"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = t(env)
    def value(env: Environment, items: Iterable[Any], f: BuiltInFunction) = items.filter{item =>
      f.run(env, item).asInstanceOf[Boolean]}
  }

  def filterNot = new OneBuiltInDeclaration{
    def name = "filterNot"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = t(env)
    def value(env: Environment, items: Iterable[Any], f: BuiltInFunction) = items.filterNot{item =>
      f.run(env, item).asInstanceOf[Boolean]}
  }


  def find = new OneBuiltInDeclaration{
    def name = "find"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) =
      ScriptDataTypeOption(tp(env))
    def value(env: Environment, items: Iterable[Any], f: BuiltInFunction) = items.find {
      item => f.run(env, item).asInstanceOf[Boolean]
    }
  }

  def isEmpty = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Seq[_]].isEmpty
    def name = "isEmpty"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeBoolean()
  }

  def size = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Seq[_]].size
    def name = "size"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeInteger()
  }
}