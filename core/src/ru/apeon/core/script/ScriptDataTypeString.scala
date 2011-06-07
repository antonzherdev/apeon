package ru.apeon.core.script

import ru.apeon.core._
import eql.SqlGeneration
import java.math.MathContext
import java.text.{DateFormatSymbols, SimpleDateFormat}
import java.util.Locale
import java.io.InputStream
import org.apache.commons.fileupload.util.Streams

case class ScriptDataTypeString() extends ScriptDataTypeSimple("string") {
  override def valueOf = {
    case i : InputStream => Streams.asString(i, "UTF-8")
    case v => v.toString}
}

object ScriptDataTypeStringDescription {
  def declarations = Seq(format, toInt, toDec0, toDec1, replace, length, substr1, substr2, pos1, pos2, toDate)

  def format = new Declaration {
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.dotRef.get.asInstanceOf[String].format(parameters.get.map{_.value} : _*)
    }
    def name = "format"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeString()
    override def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isDefined && (!parameters.get.isEmpty)
  }

  def toInt = new Declaration with SqlGeneration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      env.dotRef.get.asInstanceOf[String].toInt
    }
    def name = "toInt"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeInteger()
  def generateSql(ref: sql.Expression, parameters: Seq[sql.Expression]) = sql.Cast(ref, "int")
  }

  def toDec0 = new Declaration with SqlGeneration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      BigDecimal(env.dotRef.get.asInstanceOf[String], MathContext.DECIMAL128)
    }
    def name = "toDec"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeDecimal()
    def generateSql(ref: sql.Expression, parameters: Seq[sql.Expression]) = sql.Cast(ref, "dec")
  }

  def toDec1 = new Declaration with SqlGeneration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      BigDecimal(env.dotRef.get.asInstanceOf[String], MathContext.DECIMAL128).setScale(
        parameters.get.head.value.asInstanceOf[Int], BigDecimal.RoundingMode.FLOOR)
    }
    def name = "toDec"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeDecimal()
    override def parameters = Seq(DefPar("scale", ScriptDataTypeInteger()))
    def generateSql(ref: sql.Expression, parameters: Seq[sql.Expression]) =
      sql.Cast(ref, "dec(30, %s)".format(parameters.head))
  }

  def replace = new Declaration with SqlGeneration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
      val pars = parameters.get
      env.dotRef.get.asInstanceOf[String].replace(pars(0).value.asInstanceOf[String], pars(1).value.asInstanceOf[String])
    }
    def name = "replace"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeString()
    override def parameters = Seq(DefPar("target", ScriptDataTypeString()), DefPar("replacement", ScriptDataTypeString()))
    def generateSql(ref: sql.Expression, parameters: Seq[sql.Expression]) =
      sql.Call("replace", Seq(ref, parameters(0), parameters(1)))
  }

  def length = new Declaration with SqlGeneration{
    def name = "length"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeInteger()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[String].length()
    def generateSql(ref: sql.Expression, parameters: Seq[sql.Expression]) =
      sql.Call("length", Seq(ref))
  }

  def substr1 = new Declaration with SqlGeneration{
    def name = "sub"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeString()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[String].substring(parameters.get.head.value.asInstanceOf[Int])
    def generateSql(ref: sql.Expression, parameters: Seq[sql.Expression]) =
      sql.Call("subString", Seq(ref, sql.Plus(parameters(0), sql.Expression.constant((1)))))
    override def parameters = Seq(DefPar("start", ScriptDataTypeInteger()))
  }

  def substr2 = new Declaration with SqlGeneration{
    def name = "sub"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeString()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[String].substring(parameters.get.head.value.asInstanceOf[Int], parameters.get.apply(1).value.asInstanceOf[Int])
    def generateSql(ref: sql.Expression, parameters: Seq[sql.Expression]) =
      sql.Call("subString", Seq(ref, sql.Plus(parameters(0), sql.Expression.constant(1)), sql.Minus(parameters(1), parameters(0))))
    override def parameters = Seq(DefPar("start", ScriptDataTypeInteger()), DefPar("end", ScriptDataTypeInteger()))
  }

  def pos1 = new Declaration with SqlGeneration{
    def name = "pos"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeInteger()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[String].indexOf(parameters.get.head.value.toString)
    def generateSql(ref: sql.Expression, parameters: Seq[sql.Expression]) =
      sql.Call("locate", Seq(ref, parameters(0)))
    override def parameters = Seq(DefPar("str", ScriptDataTypeString()))
  }
  def pos2 = new Declaration with SqlGeneration{
    def name = "pos"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeInteger()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      env.ref.asInstanceOf[String].indexOf(parameters.get.head.value.toString, parameters.get.apply(1).value.asInstanceOf[Int])
    def generateSql(ref: sql.Expression, parameters: Seq[sql.Expression]) =
      sql.Call("locate", Seq(ref, parameters(0), parameters(1)))
    override def parameters = Seq(DefPar("str", ScriptDataTypeString()), DefPar("start", ScriptDataTypeInteger()))
  }

  def toDate = new Declaration with SqlGeneration{
    def name = "toDate"
    def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeDate()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) =
      new SimpleDateFormat(parameters.get.head.value.toString,
        DateFormatSymbols.getInstance(Locale.ENGLISH)).parse(env.ref.asInstanceOf[String])
    def generateSql(ref: sql.Expression, parameters: Seq[sql.Expression]) =
      sql.Call("date", Seq(ref))
    override def parameters = Seq(DefPar("format", ScriptDataTypeString()))
  }
}