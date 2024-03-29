package ru.apeon.core.script

case class Package(name : String) extends Statement with Declaration {
  override def toString = name
  def nameOf(className : String) = name match {
    case "" => className
    case _ => "%s.%s".format(name, className)
  }


  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = this
  def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypePackage(this)
  def preFillRef(env : Environment, imports: Imports) {}
  def fillRef(env: Environment, imports: Imports) {}
  def evaluate(env: Environment) = {
    env.model.addPackage(this)
    this
  }
  def dataType(env: Environment) = ScriptDataTypePackage(this)
}

object EmptyPackage extends Package("")

trait InPackage {
  def pack : Package
  def name : String
  def fullName = pack.nameOf(name)
  override val hashCode = (getClass.hashCode*13) + fullName.hashCode

  override def toString = fullName
}