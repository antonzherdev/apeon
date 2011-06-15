package ru.apeon.core.entity

import ru.apeon.core.script._
import collection.mutable.Buffer
import java.lang.RuntimeException

/**
 * Сущность
 * @param id идентификатор
 * @param name имя
 * @param table таблица
 * @param declaredColumns колонки
 * @param discriminator дискриминатор
 * @param defaultDataSourceName имя источника данных по умолчанию
 * @param extendsEntityName имя сущности, от которой отнаследована текущая сущность
 * @param declaredJoinedTables подсоединенные таблицы для мультиапдейтных сущностей
 */
case class Description(module : Module,
                       pack : Package,
                       name : String,
                       defaultDataSourceName : String,
                       table : Table,
                       declaredDeclarations : Seq[DeclarationStatement],
                       discriminator : Discriminator = DiscriminatorNull(),
                       extendsEntityName : Option[String] = None,
                       declaredJoinedTables : Seq[JoinedTable] = Seq()) extends ClassBase
{
  def attributes : Seq[Attribute] =
    fields.filter{_.isInstanceOf[Attribute]}.asInstanceOf[Seq[Attribute]]

  def ones : Seq[ToOne] =
    fields.filter{_.isInstanceOf[ToOne]}.asInstanceOf[Seq[ToOne]]

  def manies : Seq[ToMany] =
    fields.filter{_.isInstanceOf[ToMany]}.asInstanceOf[Seq[ToMany]]

  def fieldsWithSource : Seq[FieldWithSource] =
    fields.filter{_.isInstanceOf[FieldWithSource]}.asInstanceOf[Seq[FieldWithSource]]

  /**
   * Список первичных ключей
   */
  lazy val _primaryKeys : Seq[Attribute] = fields.filter{
    case a : FieldWithSource => a.isPrimaryKey //&& a.tableName.getOrElse(table.name) == table.name
    case _ => false}.asInstanceOf[Seq[Attribute]]

  def primaryKeys : Seq[FieldWithSource] = _primaryKeys

  /**
   *  Получить колонку по имени
   * @param columnName имя колонки
   * @return колонку или None
   */
  def fieldOption(name : String) : Option[Field] = fields.find{_.name == name}

  def field(name : String) : Field = fieldOption(name) match {
    case Some(column) => column
    case None => throw new NoSuchFieldException("Column \"%s\" not found in \"%s\"".format(name, fullName))
  }


  override protected def declarationsLoad = extendedDeclarations ++ super.declarationsLoad

  lazy val fields : Seq[Field] = declarations.filter(_.isInstanceOf[Field]).asInstanceOf[Seq[Field]]
  lazy val joinedTables : Seq[JoinedTable] = declaredJoinedTables ++ extendsClass.map{_.joinedTables}.getOrElse(Seq())

  def extend(declaration : DeclarationStatement) {
    extendedDeclarations.append(declaration)
  }

  private var _defaultDataSource : DataSource = _
  def defaultDataSource: DataSource = _defaultDataSource

  private var _extendsEntity : Option[Description] = _
  def extendsClass : Option[Description] = _extendsEntity

  override def toString = fullName

  def dataSource : DataSource = defaultDataSource

  override def preFillRef(env : Environment, imports: Imports) {
    _extendsEntity = extendsEntityName.map{name => env.model.entityDescription(name, Some(imports))}
    _defaultDataSource = env.model.dataSource(defaultDataSourceName, Some(imports))
    super.preFillRef(env, imports)
  }


  override def fillRef(env: Environment, imports: Imports) {
    _toString = declarations.find{
      case Def("toString", _, Seq(), _, _) => true
      case _ => false
    }.asInstanceOf[Option[Def]]
    super.fillRef(env, imports)
  }

  private var _toString : Option[Def] = _

  /**
   * Строковое представление сущности.
   * Если в сущности определена функция toString, то вызываеться она,
   * иначе сущность преобразуется в строку по стандартному алгоритму
   * @param entity сущность
   */
  def toString(entity : Entity) : String = _toString match {
    case None => entity.defaultToString
    case Some(d) => evaluateDef(entity.manager.model, d, entity).toString
  }

  private val extendedDeclarations = Buffer[DeclarationStatement]()

  override def evaluate(env: Environment) = {
    env.model.addEntityDescription(this)
    super.evaluate(env)
  }

  def dataType(env: Environment) = ScriptDataTypeEntityDescription(env.model, this)

  def elementDataType(env: Environment) = ScriptDataTypeEntityByDescription(this)

  def isInstanceOf(description : Description) : Boolean = (description == this) ||
          (extendsClass match {
            case Some(e) => e == description || e.isInstanceOf(description)
            case None => false
          })

  override def equals(obj: Any) = obj match {
    case d : Description =>
      this.module == d.module &&
      this.pack == d.pack &&
      this.name == d.name &&
      this.defaultDataSourceName == d.defaultDataSourceName &&
      this.table == d.table &&
      this.declaredDeclarations.corresponds(d.declaredDeclarations){_ == _} &&
      this.discriminator == d.discriminator &&
      this.extendsEntityName == d.extendsEntityName &&
      this.declaredJoinedTables.corresponds(d.declaredJoinedTables){_ == _}
    case _ => false
  }
}

case class DataSourceLink(dataSourceName : String) {
  private var _dataSource : DataSource = _

  def dataSource : DataSource = _dataSource

  def preFillRef(env : Environment, imports: Imports) {
    _dataSource = env.model.dataSource(dataSourceName)
  }
}

case class JoinedTable(table : Table, column : String)

case class Table(schema : String, name : String)

abstract class Field extends DeclarationStatement {
  val name : String
  def pack : Package
  def scriptDataType : ScriptDataType

  override def toString = name

  def dataType(env: Environment) = scriptDataType

  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
    if(env.ref == null) null
    else env.ref.asInstanceOf[Entity].apply(this)
}

abstract class FieldWithSource extends Field {
  def isPrimaryKey : Boolean

  def source(dataSource : DataSource) : BaseFieldSource = sources(dataSource)
  /**
   * Имя таблицы для мультиапдейтных сущностей
   */
  def tableName(dataSource : DataSource) : Option[String] = sources(dataSource).asInstanceOf[FieldSource].tableName
  def columnName(dataSource : DataSource) : String = sources(dataSource).asInstanceOf[FieldSource].columnName

  val default : Option[Default]

  val sources : FieldSources

  def isNullFor(dataSource : DataSource) : Boolean = sources(dataSource) match {
    case NullFieldSource() => true
    case _ => false
  }
}

abstract class BaseFieldSource {
  def isDefined : Boolean
  def columnName : String
}
case class FieldSource(columnName : String, tableName : Option[String] = None) extends BaseFieldSource {
  def isDefined = true
}
case class NullFieldSource() extends BaseFieldSource {
  def isDefined = false
  def columnName = throw new RuntimeException("Null field source")
}
case class FieldSources(default : BaseFieldSource, sources : Map[String, FieldSource] = Map()) {
  def apply(dataSource : DataSource) : BaseFieldSource = apply(dataSource.name)

  def apply(dataSourceName : String) : BaseFieldSource = sources.getOrElse(dataSourceName, default)
}

case class Attribute(pack : Package, name : String, sources :  FieldSources,
                     dataType : AttributeDataType,
                     default : Option[Default] = None, isPrimaryKey : Boolean = false)
        extends FieldWithSource
{

  def scriptDataType = dataType.toScriptDataType
}

case class ToOne(pack : Package, name : String, sources :  FieldSources,
                 entityName : String, default : Option[Default] = None, isPrimaryKey : Boolean = false)
        extends FieldWithSource
{
  private var _entity : Description = _
  def entity = _entity

  def scriptDataType = ScriptDataTypeEntityByDescription(entity)


  override def preFillRef(env : Environment, imports: Imports) {
    _entity = env.entityDescription(entityName, Some(imports))
  }
}

abstract class ToMany extends Field {
  def entity : Description
  def one : ToOne

  def scriptDataType = ScriptDataTypeSeq(ScriptDataTypeEntityByDescription(entity))
}

case class ToManyRef(pack : Package, name : String, entityName : String, toOneName : String) extends ToMany {
  private var _entity : Description = _

  def entity = _entity
  def one : ToOne = entity.field(toOneName).asInstanceOf[ToOne]

  override def preFillRef(env: Environment, imports: Imports) {
    _entity = env.entityDescription(entityName, Some(imports))
  }
}

case class ToManyBuiltIn(pack : Package, name : String, entity : Description) extends ToMany {
  def one : ToOne = entity.field("parent").asInstanceOf[ToOne]

  override def evaluate(env: Environment) {
    entity.evaluate(env)
  }
  override def preFillRef(env: Environment, imports: Imports) {
    entity.preFillRef(env, imports)
  }
  override def fillRef(env: Environment, imports: Imports) {
    entity.fillRef(env, imports)
  }
}

object Id extends Attribute(null, "id", FieldSources(FieldSource("id")), AttributeDataTypeInteger(), isPrimaryKey = true)

class EntityError(var s : String) extends Exception(s)

abstract class Discriminator
case class DiscriminatorNull() extends Discriminator
case class DiscriminatorColumn(sources : FieldSources, value : Any) extends Discriminator

case class ExtendEntity(module : Module, entityName : String, declarations : Seq[DeclarationStatement]) extends Statement {
  def evaluate(env: Environment) {}

  override def preFillRef(env : Environment, imports: Imports) {
    entityDescription = env.model.entityDescription(entityName, Some(imports))
    declarations.foreach{declaration =>
      entityDescription.extend(declaration)
      declaration.preFillRef(env, imports)
    }
  }

  private var entityDescription : Description = _

  def dataType(env: Environment) = ScriptDataTypeUnit()

  def fillRef(env : Environment, imports : Imports) {
    val old = env.thisType
    env.setThisType(Some(ScriptDataTypeEntityByDescription(entityDescription)))
    declarations.foreach{field =>
      field.fillRef(env, imports)
    }
    env.setThisType(old)
  }
}

abstract class Default
case class DefaultString(value : String) extends Default
case class DefaultInt(value : Int) extends Default

