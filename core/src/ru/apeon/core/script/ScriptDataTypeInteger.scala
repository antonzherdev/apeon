package ru.apeon.core.script

case class ScriptDataTypeInteger() extends ScriptDataTypeSimple("int") {
  override def valueOf = {
    case v : String => v.toInt
    case i : Int => i
  }
}

object ScriptDataTypeIntegerDeclaration{
  def declarations = Seq(between, to, to2)
  val between = new BetweenDeclaration[Int] {
    def dataType = ScriptDataTypeInteger()
    def compare(min: Int, max: Int) = min <= max
  }

  val to = new Declaration {
    def name = "to"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeSeq(ScriptDataTypeInteger())
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Int].to(parameters.get.head.value.asInstanceOf[Int])
    override def parameters = Seq(DefPar("to", ScriptDataTypeInteger()))
  }

  val to2 = new Declaration {
    def name = "to"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeSeq(ScriptDataTypeInteger())
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[Int].to(parameters.get.head.value.asInstanceOf[Int], parameters.get.apply(1).value.asInstanceOf[Int])
    override def parameters = Seq(DefPar("to", ScriptDataTypeInteger()), DefPar("step", ScriptDataTypeInteger()))
  }
}