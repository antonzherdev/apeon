package ru.apeon.core.script

import ru.apeon.core.entity.DataSource


object Package {
  def apply(model : ObjectModel, name : String, version : String, defaultDataSourceName : Option[String] = None) = {
    val ret = new Package(model, name.split('.'), Some(version.split('.').toSeq.map{_.toInt}), defaultDataSourceName)
    val env = new DefaultEnvironment
    val imports = Imports(ret)
    ret.preFillRef(model, imports)
    ret.fillRef(env, imports)
    ret.evaluate(env)
    ret
  }
}

/**
 * Пакет
 * @param model модель сущностей
 * @param name имя пакета. Например, ru.apeon.core.comtec
 * @param version версия
 * @param defaultDataSourceName имя источника данных по умолчанию.
 */
class Package(val model : ObjectModel,
              val names : Seq[String],
              val version : Option[Seq[Int]] = None,
              val defaultDataSourceName : Option[String] = None)
        extends Statement with Declaration
{
  private var _parent : Option[Package] = _
  private val _child = collection.mutable.Buffer[Package]()

  override def toString = "Package(%s, %s)".format(fullName, version)

  override def equals(obj: Any) = obj match {
    case p : Package => p.model == this.model && p.names == this.names && p.version == this.version
    case _ => false
  }

  def entityDescription(entityName : String) = model.entityDescription(entityName)

  def evaluate(env: Environment) = {
    model.addPackage(this)
    this
  }

  def dataType(env: Environment) = ScriptDataTypePackage(this)
  def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypePackage(this)

  private var _defaultDataSource : Option[DataSource] = _
  def defaultDataSource : Option[DataSource] = _defaultDataSource

  def dataSource : DataSource = defaultDataSource.getOrElse(model.defaultDataSource)

  def parent = _parent

  def fillRef(env : Environment, imports : Imports) {
    parent.foreach(_.fillRef(env, imports))
  }


  def preFillRef(model: ObjectModel, imports: Imports) {
    _defaultDataSource = defaultDataSourceName.map(name => model.dataSource(name, Some(imports)))
    if(names.size == 1){
      _parent = None
    }
    else {
      val reverseName = names.reverse
      val parentNames = reverseName.tail.reverse
      _parent = model.packOption(parentNames.mkString("."))
      if(_parent.isEmpty) {
        val par = new Package(model, parentNames)
        model.addPackage(par)
        par.preFillRef(model, imports)
        _parent = Some(par)
      }
      _parent.get._child.append(this)
    }
  }

  val name = if(names.isEmpty) "" else names.last

  def fullName = names.mkString(".")

  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = this

  def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty

  def child : Seq[Package] = _child

  def nameOf(className : String) = if(names.isEmpty) className else "%s.%s".format(fullName, className)
}

trait InPackage {
  def pack : Package
  def name : String
  def fullName = pack.nameOf(name)
}