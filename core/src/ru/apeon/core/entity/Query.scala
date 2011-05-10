package ru.apeon.core.entity

import ru.apeon.core.script._


case class Query(model : ObjectModel, module : Module, pack : Package, name : String, declaredDeclarations : Seq[DeclarationStatement],
                 extendsClass : Option[ClassBase] = None) extends ObjectBase
{
  def execute(parameters: Map[String, Any] = Map()) = {
    Script.evaluate(model, Seq(declarations.find(_.name == "apply").get.asInstanceOf[Def].statement))
  }
}






