package ru.apeon.core.script

case class ScriptDataTypeInteger() extends ScriptDataTypeSimple("int") {
  override def valueOf = {
    case v : String => v.toInt
    case i : Int => i
  }
}

object ScriptDataTypeIntegerDeclaration{
  def declarations = Seq(between)
  val between = new BetweenDeclaration[Int] {
    def dataType = ScriptDataTypeInteger()
    def compare(min: Int, max: Int) = min <= max
  }
}