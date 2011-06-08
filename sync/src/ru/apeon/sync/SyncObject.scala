package ru.apeon.sync

import ru.apeon.core.loader.Loader
import java.io.File
import xml.XML
import java.text.SimpleDateFormat
import ru.apeon.core.script._
import java.util.{Calendar, Date}

object SyncObject extends ObjectBase {
  val InsertOnly = 1
  val NoAutoUpdate = 2
  val NoAutoUpdateToOne = 4
  val NoAutoUpdateToMany = 8
  val ToManyAppend = 16
  val UpdateOnly = 32

  def module = CoreModule
  def pack = EmptyPackage

  def name = "Sync"
  def extendsClass = None
  def declaredDeclarations = Seq(
    ConstDeclaration("InsertOnly", InsertOnly),
    ConstDeclaration("NoAuto", NoAutoUpdate),
    ConstDeclaration("NoAutoToOne", NoAutoUpdateToOne),
    ConstDeclaration("NoAutoToMany", NoAutoUpdateToMany),
    ConstDeclaration("ToManyAppend", ToManyAppend),
    ConstDeclaration("UpdateOnly", UpdateOnly),
    SyncDate
  )

  case class ConstDeclaration(name : String, value : Int) extends DeclarationStatement {
    def dataType(env: Environment) = ScriptDataTypeInteger()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = this.value
  }

  object SyncDate extends DeclarationStatement {
    val xmlFile = new File(Loader.persistDirectory, "syncDates.xml")

    val format = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss")
    lazy val dates : collection.mutable.Map[String, Date] = if(!xmlFile.exists){
      collection.mutable.Map.empty[String, Date]
    } else {
      val b = collection.mutable.Map.newBuilder[String, Date]
      (XML.loadFile(xmlFile)\\"date").foreach { date =>
        b += (date\"@name").text -> format.parse((date\"@value").text)
      }
      b.result()
    }

    def save() {
       val xml =
         <dates>
           {
            dates.map{date =>
              <date name={date._1} value={format.format(date._2)}/>
            }
           }
         </dates>

      XML.save(xmlFile.getAbsolutePath, xml)
    }

    def dataType(env: Environment) = ScriptDataTypeUnit()
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression])  = {
      parameters match {
        case Some(Seq(ParVal(name : String, _), ParVal(f : BuiltInFunction, _))) =>
          val date = dates.getOrElse(name, {
            val cal = Calendar.getInstance()
            cal.set(0, 0, 0)
            cal.getTime
          })
          val ret = f.run(env, date)
          dates.update(name, Calendar.getInstance().getTime)
          save()
          ret
        case _ => throw ScriptException(env, "Error in parameters.")
      }
    }
    def name = "sync"
    override def parameters = Seq(DefPar("name", ScriptDataTypeString()), DefPar("f", ScriptDataTypeBuiltInFunction()))

    override def builtInParameters(env: Environment, parameters: Option[Seq[Par]], parameterNumber: Int, parameter: Par) =
      Seq(BuiltInParameterDef("syncDate", ScriptDataTypeDate()))
  }
}