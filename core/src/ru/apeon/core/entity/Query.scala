package ru.apeon.core.entity

import ru.apeon.core.script._


case class Query(model : ObjectModel, module : Module, pack : Package, name : String, declaredDeclarations : Seq[DeclarationStatement],
                 extendsClass : Option[ClassBase] = None) extends ObjectBase
{
  def execute(parameters: Map[String, String] = Map()) {

    val apply = declarations.find{
      case dec : Def =>
        dec.name == "apply" && parameters.size == dec.parameters.size && parameters.forall{
          par => dec.parameters.find{_.name == par._1}.isDefined
        }
      case _ => false
    }.get.asInstanceOf[Def]
    val e = new DefaultEnvironment(model)
    e.start()
    val ret = e.atomic{
      apply.value(e, apply.parameters.map {
        par => ParVal(par.dataType.valueOf(parameters(par.name)), Some(par.name))
      } match {
        case Seq() => None
        case s => Some(s)
      })
    }
    e.end()
    ret
  }
}






