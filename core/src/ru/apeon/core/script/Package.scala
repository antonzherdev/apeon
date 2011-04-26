package ru.apeon.core.script

object Package {
  def apply(model : ObjectModel, fullName : String) : Package = apply(model, fullName.split('.').toSeq)

  def apply(model : ObjectModel, names : Seq[String]) : Package = {
    model.packOption(names.mkString(".")).getOrElse{
      val pack = names match {
        case Seq() => new RootPackage(model)
        case _ => new NonRootPackage(model, Package(model, names.take(names.size - 1).mkString(".")), names.last)
      }
      model.addPackage(pack)
      pack
    }
  }
}

trait Package extends Declaration{
  def model : ObjectModel
  def nameOf(className : String) : String
  def name : String
  def fullName : String

  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = this
  def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
  def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypePackage(this)
}

class RootPackage(val model : ObjectModel) extends Package {
  def nameOf(className: String) = className
  def name = ""
  def fullName = ""
}

class NonRootPackage(val model : ObjectModel,
              val pack : Package,
              val name : String)
        extends Package with Statement with InPackage
{
override def equals(obj: Any) = obj match {
    case p : NonRootPackage => p.model == this.model && p.name == this.name && p.pack == this.pack
    case _ => false
  }
  def dataType(env: Environment) = ScriptDataTypePackage(this)
  def evaluate(env: Environment) = this
  def fillRef(env : Environment, imports : Imports) {}
  def preFillRef(model: ObjectModel, imports: Imports) {}

  def nameOf(className : String) = "%s.%s".format(fullName, className)
}

trait InPackage {
  def pack : Package
  def name : String
  def fullName = pack.nameOf(name)

  override def toString = fullName
}