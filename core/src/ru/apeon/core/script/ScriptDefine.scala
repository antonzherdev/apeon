package ru.apeon.core.script

/**
 * @author Anton Zherdev
 */

trait ScriptDefine {
  implicit def expressionToSeqPar(e: Expression) : Option[Seq[Par]] = Some(Seq(Par(e)))

  implicit def stringToConst(s: String) : ConstString = ConstString(s)
  implicit def intToConst(i: Int) : ConstInt = ConstInt(i)
  implicit def parToSeqPar(par: Par) : Option[Seq[Par]] = Some(Seq(par))

  def ref(name : String, parameters : Expression*) = Ref(name, Some(parameters.toSeq.map(Par(_))))
}