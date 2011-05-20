package ru.apeon.core.eql

trait EqlDefine {
  implicit def toRef(s : String) : Expression = Ref(s)
  implicit def toExpression(d : ExpressionBuilder) : Expression = d.expression
  implicit def toBuilder(d : Expression) : ExpressionBuilder = new ExpressionBuilder(d)
  class ExpressionBuilder(val expression : Expression) {
    def ~(right : Ref) = new ExpressionBuilder(Dot(expression, right))
    def ===(right : Expression) = new ExpressionBuilder(Equal(expression, right))
  }
}