package ru.apeon.core.script

import java.util.{Calendar, Date}
import java.text.SimpleDateFormat

case class ScriptDataTypeDate() extends ScriptDataTypeSimple("date") {
  val valueOfFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  override def valueOf(str: String) = valueOfFormat.parse(str)
}

object ScriptDataTypeDateDescription {
  def declarations = Seq(
    AddFunction("addDays", Calendar.DATE),
    AddFunction("addMonths", Calendar.MONTH),
    AddFunction("addYears", Calendar.YEAR),
    AddFunction("addSeconds", Calendar.SECOND),
    AddFunction("addMinutes", Calendar.MINUTE),
    AddFunction("addHours", Calendar.HOUR),
    daysTo
  )

  case class AddFunction(name : String, field : Int) extends Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      val cal = Calendar.getInstance
      cal.setTime(env.ref.asInstanceOf[Date])
      cal.add(field, parameters.get.head.value.asInstanceOf[Int])
      cal.getTime
    }

    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeDate()
    override def parameters = Seq(DefPar("value", ScriptDataTypeInteger()))
  }

  def daysTo = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      var start = env.ref.asInstanceOf[Date]
      val end = parameters.get.apply(0).value.asInstanceOf[Date]
      val cal = Calendar.getInstance

      val b = Seq.newBuilder[Date]
      while(start.compareTo(end) < 1) {
        b += start
        cal.setTime(start)
        cal.add(Calendar.DATE, 1)
        start = cal.getTime
      }
      b.result()
    }
    def name = "daysTo"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeSeq(ScriptDataTypeDate())
    override def parameters = Seq(DefPar("to", ScriptDataTypeDate()))
  }
}