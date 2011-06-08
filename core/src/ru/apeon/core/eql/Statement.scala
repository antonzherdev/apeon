package ru.apeon.core.eql

import java.lang.String
import ru.apeon.core._
import ru.apeon.core.entity._

abstract class Statement {
  fillRef()

  def eq[E](a : Option[E], b: Option[E]) = a match {
    case None => b.isEmpty
    case Some(e) => b.isDefined && b.get.equals(e)
  }

  protected def fillRef() {
    fillRef(new DefaultEnvironment(EntityConfiguration.model))
  }

  protected def fillRef(env : Environment)

  def dataSource : DataSource
}

case class Select(from : From,
                  columns : Seq[Column] = Seq(),
                  where : Option[Expression] = None,
                  orderBy : Seq[OrderBy] = Seq())
        extends Statement
{
  def dataSource = from.dataSource

  protected def fillRef(env: Environment) {
    env.push(from)
    from.fillRef(env)
    columns.foreach(_.expression.fillRef(env))
    if(where.isDefined)
      where.get.fillRef(env)
    orderBy.foreach(_.expression.fillRef(env))
    env.pop()
  }

  def evaluate = dataSource.store.transaction{
    dataSource.store.select(this)
  }
}

case class Column(expression : Expression, name : String)

trait From {
  def name : String
  def field(name : String) : Field = fieldOption(name).getOrElse{
    throw new RuntimeException("Column \"%s\" not found in %s.".format(name, this))}
  def fieldOption(name : String) : Option[Field]
  def fields : Seq[Field]
  def dataSource : DataSource
  def fillRef(env: Environment)

  def fromOption(alias : String) : Option[From] =
    if(alias == name)
      Some(this)
    else
      None

  def from(alias : String) : From = fromOption(alias).getOrElse{
    throw new RuntimeException("Alias \"%s\" not found in %s.".format(name, this))}
}

object From {
  def apply(entity : Description) : FromEntity = FromEntity(entity)
  def apply(entity : Description, alias : String) : FromEntity = FromEntity(entity, Some(alias))
}

case class FromEntity(entity : Description,
                      alias : Option[String] = None,
                      dataSourceExpression : DataSourceExpression = DataSourceExpressionDefault()) extends From
{
  def fieldOption(name: String) = entity.fieldOption(name)
  def fields = entity.fields

  override def toString = {
    "from " + entity.name +
            (if(dataSourceExpression.isDefined) "<" + dataSourceExpression.toString + ">" else "") +
            (if(alias.isDefined) " as " + alias.get else "")
  }

  def dataSource = dataSourceExpression.dataSource(entity)

  def name = alias match {
    case Some(a) => a
    case None => entity.name
  }

  def fillRef(env: Environment) {
  }
}

abstract class DataSourceExpression {
  def dataSource(entity : Description) : DataSource
  def isDefined : Boolean
}

case class DataSourceExpressionDataSource(dataSource : DataSource) extends DataSourceExpression{
  def dataSource(entity : Description) = dataSource
  def isDefined = true
  override def toString = dataSource.name
}

case class DataSourceExpressionDefault() extends DataSourceExpression {
  def dataSource(entity: Description) = entity.dataSource
  def isDefined = true
}



case class FromToMany(ref : Expression, alias : Option[String])
        extends From
{
   def name = alias match {
    case Some(a) => a
    case None => "_"
  }

  private var _entity : Description = _
  def entity = _entity
  def fields = _entity.fields
  def fieldOption(name: String) = _entity.fieldOption(name)

  private var _toMany : ToMany = _
  def toMany : ToMany = _toMany

  override def fillRef(env: Environment) {
    ref.fillRef(env)
    _entity = ref.dataType() match {
      case script.ScriptDataTypeSeq(e : script.ScriptDataTypeEntity) => e.description
      case _ => throw new RuntimeException("Unsupported datatype")
    }
    _toMany = ref match {
      case r : Ref => r.declaration.asInstanceOf[ToMany]
      case d : Dot => d.right.declaration.asInstanceOf[ToMany]
    }
  }


  def dataSource = null
}



abstract class Direction
case class Asc() extends Direction {
  override def toString = "asc"
}
case class Desc() extends Direction {
  override def toString = "desc"
}

case class OrderBy(expression : Expression, direction : Direction = Asc())

case class Delete(from : FromEntity, var where : Option[Expression] = None) extends Statement {
  def fillRef(env: Environment) {
    env.push(from)
    from.fillRef(env)
    if(where.isDefined)
      where.get.fillRef(env)
    env.pop()
  }

  def dataSource = from.dataSource
}

case class Insert(from : FromEntity, columns : Seq[InsertColumn]) extends Statement {
  def fillRef(env: Environment) {
    env.push(from)
    columns.foreach(_.fillRef(env))
    env.pop()
  }

  def dataSource = from.dataSource
}

case class InsertColumn(columnName : String,  expression : Expression) {
  var column : FieldWithSource = null
  def fillRef(env: Environment) {
    column = env.from.field(columnName).asInstanceOf[FieldWithSource]
    expression.fillRef(env)
  }

  override def toString = "%s = %s".format(columnName, expression)
}

case class Update(from : FromEntity, columns : Seq[UpdateColumn], where : Option[Expression] = None) extends Statement {
  def fillRef(env: Environment) {
    env.push(from)
    from.fillRef(env)
    columns.foreach(_.fillRef(env))
    if(where.isDefined)
      where.get.fillRef(env)
    env.pop()
  }

  def dataSource = from.dataSource
}

case class UpdateColumn(columnName : String, expression : Expression) {
  var column : FieldWithSource = null
  def fillRef(env: Environment) {
    column = env.from.field(columnName).asInstanceOf[FieldWithSource]
    expression.fillRef(env)
  }
}