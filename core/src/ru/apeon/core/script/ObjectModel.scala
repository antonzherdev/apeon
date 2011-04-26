package ru.apeon.core.script

import scala.collection.mutable.Map
import ru.apeon.core.entity._

/**
 * Класс содержит схему сущностей.
 */
trait ObjectModel {
  def entityDescriptionOption(name :String, env : Option[Imports] = None) : Option[Description]

  def entityDescription(name :String, env : Option[Imports] = None) : Description =
    entityDescriptionOption(name, env).getOrElse(
      throw new RuntimeException("Entity \"%s\" not found.".format(name))
    )

  def addEntityDescription(description : Description)

  def objOption(name : String, imports : Option[Imports] = None) : Option[ObjectBase]

  def obj(name : String, imports : Option[Imports] = None) : ObjectBase =  objOption(name, imports).getOrElse(
    throw new RuntimeException("Object \"%s\" not found.".format(name))
  )

  def addObj(query : ObjectBase)

  def dataSourceOption(name : String, imports : Option[Imports] = None) : Option[DataSource]
  def dataSource(name : String, imports : Option[Imports] = None) = dataSourceOption(name, imports).getOrElse(
    throw new RuntimeException("Data source \"%s\" not found.".format(name))
  )
  def addDataSource(dataSource : DataSource)

  def addPackage(pack : Package)

  def pack(name : String, imports : Option[Imports] = None) = packOption(name, imports).getOrElse(
    throw new RuntimeException("Package \"%s\" not found.".format(name))
  )
  def packOption(name : String, imports : Option[Imports] = None) : Option[Package]

  protected def loadDefaultObjects() {
    addObj(DateObject(Package("")))
    addObj(LogObject(Package("")))
  }
}

case class Imports(pack : Package, imports : Seq[String] = Seq())

class DefaultObjectModel extends ObjectModel{
  private val entities = Map[String, Description]()
  private val queries = Map[String, ObjectBase]()
  private val dataSources = Map[String, DataSource]()
  private val packages = Map[String, Package]()

  def entityDescriptionOption(name: String, env : Option[Imports]) =
    get(entities, name, env)

  def addEntityDescription(description : Description) {
    entities.update(description.fullName, description)
  }

  def addObj(query : ObjectBase) {
    queries.update(query.fullName, query)
  }

  def objOption(name: String, env : Option[Imports]) = get(queries, name, env)

  def dataSourceOption(name: String, env : Option[Imports]) = get(dataSources, name, env)

  def addDataSource(dataSource: DataSource) {
    dataSources.update(dataSource.fullName, dataSource)
  }

  def packOption(name: String, imports: Option[Imports]) = get(packages, name, imports)

  def addPackage(pack: Package) {
    var name = ""
    for(n <- pack.name.split('.')) {
      name = name + n
      packages.getOrElseUpdate(name, Package(name))
      name = name + "."
    }
  }

  private def get[T](map : Map[String, T], name : String, env : Option[Imports]) : Option[T] = {
    var ret = map.get(name)
    if(ret.isEmpty && env.isDefined) {
      ret = map.get(env.get.pack.name + "." + name)
      if(ret.isEmpty) {
        val i = env.get.imports.iterator
        while(i.hasNext && ret.isEmpty) {
          val imp = i.next()
          if(imp.last == '_') {
            ret = map.get(imp.substring(0, imp.length - 1) + name)
          } else {
            if(imp.endsWith("." + name)) {
              ret = map.get(imp)
            }
          }

        }
      }
    }
    ret
  }
  loadDefaultObjects()
}
