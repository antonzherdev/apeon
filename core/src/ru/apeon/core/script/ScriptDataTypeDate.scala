package ru.apeon.core.script

import java.util.{Calendar, Date}
import java.text.SimpleDateFormat

case class ScriptDataTypeDate() extends ScriptDataTypeSimple("date") {
  override def declarations = Seq(
    AddFunction("addDays", Calendar.DATE),
    AddFunction("addMonths", Calendar.MONTH),
    AddFunction("addYears", Calendar.YEAR),
    AddFunction("addSeconds", Calendar.SECOND),
    AddFunction("addMinutes", Calendar.MINUTE),
    AddFunction("addHours", Calendar.HOUR)
  )

  case class AddFunction(name : String, field : Int) extends Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      val cal = Calendar.getInstance
      cal.setTime(env.ref.asInstanceOf[Date])
      cal.add(field, parameters.get.head.value.asInstanceOf[Int])
      cal.getTime
    }

    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeDate()
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters match {
      case Some(Seq(p)) => p.expression.dataType(env) == ScriptDataTypeInteger()
      case _ => false
    }
  }

  val valueOfFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  override def valueOf(str: String) = valueOfFormat.parse(str)
}