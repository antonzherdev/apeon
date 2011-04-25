package ru.apeon.core.entity

import ru.apeon.core.script._


case class Query(pack : Package, name : String, declaredDeclarations : Seq[DeclarationStatement],
                 extendsClass : Option[ClassBase] = None) extends ObjectBase
{
  def execute(parameters: Map[String, Any] = Map()) = {
    Script.evaluate(pack, Seq(declarations.find(_.name == "apply").get.asInstanceOf[Def].statement))
  }
}






