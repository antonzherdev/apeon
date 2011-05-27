package ru.apeon.core.eql

import collection.mutable.Buffer
import ru.apeon.core.entity.{DataSource, EntityDefine}

trait EqlDefine extends EntityDefine {
  implicit def toRef(s : String) : Ref = Ref(s)
  implicit def toConst(i : Int) : Expression = Const.apply(i)
  implicit def toExpression(d : ExpressionBuilder) : Expression = d.expression
  implicit def toBuilder(d : Expression) : ExpressionBuilder = new ExpressionBuilder(d)
  implicit def toBuilder(d : String) : ExpressionBuilder = new ExpressionBuilder(Ref(d))
  class ExpressionBuilder(val expression : Expression) {
    def ~(right : Ref) = new ExpressionBuilder(Dot(expression, right))
    def ===(right : Expression) = new ExpressionBuilder(Equal(expression, right))
  }

  def from(entity : String) : SelectBuilder =
    new SelectBuilder(FromEntity(des(entity)))

  def from(entity : String, ds : DataSource) : SelectBuilder =
    new SelectBuilder(FromEntity(des(entity), None, DataSourceExpressionDataSource(ds)))

  implicit def toSelect(b : SelectBuilder) : Select = b.b
  class SelectBuilder(from : From) {
    val columns = Buffer.empty[Column]
    var where : Option[Expression] = None

    def b : Select = Select(from, columns.toSeq, where)

    def col(exp : Expression, name : String) : SelectBuilder = {
      columns.append(Column(exp, name))
      this
    }

    def where(exp : Expression) : SelectBuilder = {
      where = Some(exp)
      this
    }
  }

  def update(entity : String) : UpdateBuilder =
    new UpdateBuilder(FromEntity(des(entity)))

  def update(entity : String, ds : DataSource) : UpdateBuilder =
    new UpdateBuilder(FromEntity(des(entity), None, DataSourceExpressionDataSource(ds)))

  implicit def toUpdate(b : UpdateBuilder) : Update = b.b
  class UpdateBuilder(from : FromEntity) {
    val columns = Buffer.empty[UpdateColumn]
    var where : Option[Expression] = None

    def b : Update = Update(from, columns.toSeq, where)

    def set(name : String, exp : Expression) : UpdateBuilder = {
      columns.append(UpdateColumn(name, exp))
      this
    }

    def where(exp : Expression) : UpdateBuilder = {
      where = Some(exp)
      this
    }
  }

  def insert(entity : String) : InsertBuilder =
    new InsertBuilder(FromEntity(des(entity)))

  def insert(entity : String, ds : DataSource) : InsertBuilder =
    new InsertBuilder(FromEntity(des(entity), None, DataSourceExpressionDataSource(ds)))

  implicit def toInsert(b : InsertBuilder) : Insert = b.b
  class InsertBuilder(from : FromEntity) {
    val columns = Buffer.empty[InsertColumn]

    def b : Insert = Insert(from, columns.toSeq)

    def set(name : String, exp : Expression) : InsertBuilder = {
      columns.append(InsertColumn(name, exp))
      this
    }
  }
}