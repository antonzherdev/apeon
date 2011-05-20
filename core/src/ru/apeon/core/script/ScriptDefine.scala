package ru.apeon.core.script

trait ScriptDefine {
  implicit def expressionToSeqPar(e: Expression) : Option[Seq[Par]] = Some(Seq(Par(e)))

  implicit def stringToConst(s: String) : ConstString = ConstString(s)
  implicit def intToConst(i: Int) : ConstInt = ConstInt(i)
  implicit def parToSeqPar(par: Par) : Option[Seq[Par]] = Some(Seq(par))

  def ref(name : String, parameters : Expression*) = Ref(name, parameters.toSeq.map(Par(_)) match {
    case Seq() => None
    case s => Some(s)
  })

  implicit def toExpression(d : ExpressionBuilder) : Expression = d.expression
  implicit def toBuilder(d : Expression) : ExpressionBuilder = new ExpressionBuilder(d)

  def Eql(s : String) = ConstEql(s)

  def bf(s : Statement*) = BuiltInFunction(Parentheses(s.toSeq))

  class ExpressionBuilder(val expression : Expression) {
    def ~(right : Ref) = new ExpressionBuilder(Dot(expression, right))
    def :=(right : Expression) = new ExpressionBuilder(Set(expression, right))
    def +(right : Expression) = new ExpressionBuilder(Plus(expression, right))
  }
}