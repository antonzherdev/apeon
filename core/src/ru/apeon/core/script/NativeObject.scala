package ru.apeon.core.script

object NativeObject extends ObjectBase  {
  def name = "Native"
  def module = CoreModule
  def pack = EmptyPackage
  def extendsClass = None

  def declaredDeclarations = Seq(new DeclarationStatement {
    def name = "value"
    def dataType(env: Environment) = ScriptDataTypeAny()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.dataSource(dataSource).get.store.nativeOne(parameters.get.head.value.asInstanceOf[String]).get
    }
    override def parameters = Seq(DefPar("query", ScriptDataTypeString()))
  })
}