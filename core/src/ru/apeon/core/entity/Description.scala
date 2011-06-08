package ru.apeon.core.entity

import ru.apeon.core.script._
import collection.mutable.Buffer


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
case class Description(pack : Package,
                       name : String,
                       table : Table,
                       declaredDeclarations : Seq[DeclarationStatement],
                       discriminator : Discriminator = DiscriminatorNull(),
                       defaultDataSourceName : Option[String] = None,
                       extendsEntityName : Option[String] = None,
                       declaredJoinedTables : Seq[JoinedTable] = Seq()) extends ClassBase
{
  def attributes : Seq[Attribute] =
    fields.filter{_.isInstanceOf[Attribute]}.asInstanceOf[Seq[Attribute]]

  def columnsWithColumn : Seq[FieldWithSource] =
    fields.filter{_.isInstanceOf[FieldWithSource]}.asInstanceOf[Seq[FieldWithSource]]

  /**
   * Список первичных ключей
   */
  lazy val _primaryKeys : Seq[Attribute] = fields.filter{
    case a : Attribute => a.isPrimaryKey //&& a.tableName.getOrElse(table.name) == table.name
    case _ => false}.asInstanceOf[Seq[Attribute]]

  def primaryKeys : Seq[Attribute] = _primaryKeys

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


  override protected def declarationsLoad =
    super.declarationsLoad ++ extendedFields

  lazy val fields : Seq[Field] = declarations.filter(_.isInstanceOf[Field]).asInstanceOf[Seq[Field]]
  lazy val joinedTables : Seq[JoinedTable] = declaredJoinedTables ++ extendsClass.map{_.joinedTables}.getOrElse(Seq())

  def extend(field : Field) {
    extendedFields.append(field)
  }

  def model = pack.model

  private var _defaultDataSource : Option[DataSource] = _
  def defaultDataSource: Option[DataSource] = _defaultDataSource

  private var _extendsEntity : Option[Description] = _
  def extendsClass : Option[Description] = _extendsEntity

  override def toString = fullName

  def dataSource : DataSource = defaultDataSource.getOrElse(pack.dataSource)


  override def preFillRef(model : ObjectModel, imports: Imports) {
    _extendsEntity = extendsEntityName.map{name => model.entityDescription(name, Some(imports))}
    _defaultDataSource = defaultDataSourceName.map(name => model.dataSource(name, Some(imports)))
    super.preFillRef(model, imports)
  }


  override def fillRef(env: Environment, imports: Imports) {
    _toString = declarations.find{
      case Def("toString", _, Seq(), _) => true
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
    case Some(d) => evaluateDef(d, entity).toString
  }

  private val extendedFields = Buffer[Field]()

  def evaluate(env: Environment) {
    env.model.addEntityDescription(this)
    this
  }

  def dataType(env: Environment) = ScriptDataTypeEntityDescription(this)

  def elementDataType(env: Environment) = ScriptDataTypeEntityByDescription(this)

  def isInstanceOf(description : Description) : Boolean = (description == this) ||
          (extendsClass match {
            case Some(e) => e == description || e.isInstanceOf(description)
            case None => false
          })
}

case class JoinedTable(table : Table, column : String)

case class Table(schema : String, name : String)

abstract class Field extends DeclarationStatement {
  val name : String
  def pack : Package
  def scriptDataType : ScriptDataType

  override def toString = name

  def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty

  def dataType(env: Environment) = scriptDataType

  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
    if(env.ref == null) null
    else env.ref.asInstanceOf[Entity].apply(this)
}

abstract class FieldWithSource extends Field {
  /**
   * Имя таблицы для мультиапдейтных сущностей
   */
  def tableName(dataSource : DataSource) : Option[String] = sources(dataSource).tableName
  def columnName(dataSource : DataSource) : String = sources(dataSource).columnName

  val default : Option[Default]

  val sources : FieldSources
}

case class FieldSource(columnName : String, tableName : Option[String] = None)
case class FieldSources(default : FieldSource, sources : Map[String, FieldSource] = Map()) {
  def apply(dataSource : DataSource) : FieldSource = apply(dataSource.name)

  def apply(dataSourceName : String) : FieldSource = sources.getOrElse(dataSourceName, default)
}

case class Attribute(pack : Package, name : String, sources :  FieldSources,
                     dataType : AttributeDataType,
                     default : Option[Default] = None, isPrimaryKey : Boolean = false)
        extends FieldWithSource
{

  def scriptDataType = dataType.toScriptDataType
}

case class ToOne(pack : Package, name : String, sources :  FieldSources,
                 entityName : String, default : Option[Default] = None)
        extends FieldWithSource
{
  private var _entity : Description = _
  def entity = _entity

  def scriptDataType = ScriptDataTypeEntityByDescription(entity)


  override def preFillRef(model: ObjectModel, imports: Imports) {
    _entity = model.entityDescription(entityName, Some(imports))
  }

}

case class ToMany(pack : Package, name : String, entityName : String, toOneName : String) extends Field {
  val isPrimaryKey = false

  private var _entity : Description = _

  def entity = _entity
  def toOne : ToOne = entity.field(toOneName).asInstanceOf[ToOne]

  def scriptDataType = ScriptDataTypeSeq(ScriptDataTypeEntityByDescription(entity))


  override def preFillRef(model: ObjectModel, imports: Imports) {
    _entity = model.entityDescription(entityName, Some(imports))
  }
}

object Id extends Attribute(null, "id", FieldSources(FieldSource("id")), AttributeDataTypeInteger(), isPrimaryKey = true)

class EntityError(var s : String) extends Exception(s)

abstract class Discriminator
case class DiscriminatorNull() extends Discriminator
case class DiscriminatorColumn(columnName : String, value : String) extends Discriminator

case class ExtendEntity(entityName : String, fields : Seq[Field]) extends Statement {
  def evaluate(env: Environment) {}


  override def preFillRef(model: ObjectModel, imports: Imports) {
    entityDescription = model.entityDescription(entityName, Some(imports))
    fields.foreach{field =>
      entityDescription.extend(field)
      field.preFillRef(model, imports)
    }
  }

  private var entityDescription : Description = _

  def dataType(env: Environment) = ScriptDataTypeUnit()

  def fillRef(env : Environment, imports : Imports) {
    fields.foreach{field =>
      field.fillRef(env, imports)
    }
  }
}

abstract class Default
case class DefaultString(value : String) extends Default
case class DefaultInt(value : Int) extends Default

trait EntityDefine {
  implicit def string2EntityColumnSources(columnName : String) : FieldSources =
    FieldSources(FieldSource(columnName))

  implicit def string2EntityColumnSources(source : (String, String)) : FieldSources =
    FieldSources(FieldSource(source._2, Some(source._1)))

  implicit def string2Table(columnName : String) : Table =
    Table("", columnName)

}
