package ru.apeon.core.script

trait ClassBase extends Declaration with Statement with InPackage {
  def declaredDeclarations : Seq[DeclarationStatement]
  def extendsClass : Option[ClassBase]
  def pack : Package
  def module : Module


  protected def declarationsLoad : Seq[Declaration] = declaredDeclarations ++ extendsClass.map{_.declarations}.getOrElse(Seq())
  private lazy val _declarations : Seq[Declaration] = declarationsLoad
  def declarations : Seq[Declaration] = _declarations
  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = this

  override def preFillRef(env : Environment, imports: Imports) {
    declaredDeclarations.foreach(dec => env.preFillRef(dec, imports))
  }

  def fillRef(env: Environment, imports: Imports) {
    env.withThisType(elementDataType(env)) {
      declaredDeclarations.foreach {
        dec =>
          env.atomic {
            env.fillRef(dec, imports)
          }
      }
    }
  }
  def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
  def elementDataType(env: Environment) : ScriptDataType

  /**
   * Выполнить функцию для объекта
   * @param function функция
   * @param obj объект
   * @return результат выполнения функции
   */
  def evaluateDef(model : ObjectModel, function : Def, obj : Any) : Any = {
    val env = new DefaultEnvironment(model)
    env.withThisRef(Some(obj)) {
      Script.evaluate(env, Seq(function.statement))
    }
  }

  def dataType(env: Environment, parameters: Option[Seq[Par]]) = dataType(env)
}

trait ObjectBase extends ClassBase  {
  def dataType(env: Environment) = ScriptDataTypeObject(this)
  def elementDataType(env: Environment) : ScriptDataType = dataType(env)

  def evaluate(env: Environment) {
    env.model.addObj(this)
  }
}




case class Object(module : Module, pack : Package, name : String, declaredDeclarations : Seq[DeclarationStatement],
                  extendsClass : Option[ClassBase] = None) extends ObjectBase