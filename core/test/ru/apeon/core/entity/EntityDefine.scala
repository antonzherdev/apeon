package ru.apeon.core.entity

import ru.apeon.core.script._
import collection.mutable.Buffer

trait EntityDefine {
  var _model = new DefaultObjectModel
  def model = _model
  val _pack = Package("ru.apeon.core")
  def pack = _pack
  val _dataSource = createDataSource
  def dataSource = _dataSource

  def createDataSource : DataSource = DataSource(pack, "ds")

  def des(entityName : String) : Description = model.entityDescription(entityName, Some(Imports(pack)))

  def clearModel() {
    _model = new DefaultObjectModel
    init()
  }
  init()

  def pack(name : String) = {
    val ret = Package(name)
    model.addPackage(ret)
    ret
  }

  def init() {
    EntityConfiguration.model = model
    model.addPackage(pack)
    model.addDataSource(dataSource)
  }

  def fillRef() {
    val e = new DefaultEnvironment(model)
    /*model.packs.foreach(_.evaluate(e))
    model.objs.foreach(_.evaluate(e))
    model.entityDescriptions.foreach(_.evaluate(e))
    model.dataSources.foreach(_.evaluate(e))       */

    model.packs.foreach(pack => pack.preFillRef(e, Imports(pack)))
    model.objs.foreach(o => o.preFillRef(e, Imports(o.pack)))
    model.entityDescriptions.foreach(o => o.preFillRef(e, Imports(o.pack)))
    model.dataSources.foreach(o => o.preFillRef(e, Imports(o.pack)))

    model.packs.foreach(pack => pack.fillRef(e, Imports(pack)))
    model.objs.foreach(o => o.fillRef(e, Imports(o.pack)))
    model.entityDescriptions.foreach(o => o.fillRef(e, Imports(o.pack)))
    model.dataSources.foreach(o => o.fillRef(e, Imports(o.pack)))
  }

  implicit def string2EntityColumnSources(columnName : String) : FieldSources =
    FieldSources(FieldSource(columnName))

  implicit def string2EntityColumnSources(source : (String, String)) : FieldSources =
    FieldSources(FieldSource(source._2, Some(source._1)))

  def desc(name : String) = new DescriptionBuilder(name)

  def att(name : String, dataType : AttributeDataType) = Attribute(pack, name, FieldSources(FieldSource(name)), dataType)
  def int = AttributeDataTypeInteger()
  def id = Id
  def one(name : String, entityName : String) = ToOne(pack, name, FieldSources(FieldSource("id_" + name)), entityName)

  class DescriptionBuilder(val name : String) {
    private var module : Module = CoreModule
    private var pack : Package = EntityDefine.this.pack
    private var defaultDataSourceName : String = "ds"
    private var table : Option[Table] = None
    private val declaredDeclarations = Buffer[DeclarationStatement]()
    private var discriminator : Discriminator = DiscriminatorNull()
    private var extendsEntityName : Option[String] = None
    private var declaredJoinedTables = Buffer[JoinedTable]()

    def b = {
      val ret = new Description(module, pack, name, defaultDataSourceName, table.getOrElse(Table("", name)),
        declaredDeclarations, discriminator, extendsEntityName, declaredJoinedTables)
      model.addEntityDescription(ret)
      ret
    }


    def in_package(pack : Package) = {
      this.pack = pack
      this
    }

    def in_module(module : Module) = {
      this.module = module
      this
    }

    def table(table : Table) : DescriptionBuilder = {
      this.table = Some(table)
      this
    }
    def table(name : String) : DescriptionBuilder = table(Table("", name))
    def table(schema : String, name : String) : DescriptionBuilder = table(Table(schema, name))

    def decl(d : DeclarationStatement*) = {
      declaredDeclarations.append(d : _*)
      this
    }

    def ext(name : String) = {
      extendsEntityName = Some(name)
      this
    }

    def discriminator(column : String, value : Any) : DescriptionBuilder = {
      this.discriminator = DiscriminatorColumn(column, value)
      this
    }

    def join(schema : String, table : String, column : String) = {
      declaredJoinedTables.append(JoinedTable(Table(schema, table), column))
      this
    }

    def ds(name : String ) = {
      defaultDataSourceName = name
      this
    }
  }

  def withModel(define :  => Unit) {
    clearModel()
    define
    fillRef()
  }
}
