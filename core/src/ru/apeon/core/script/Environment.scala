package ru.apeon.core.script

import collection.mutable.Stack
import ru.apeon.core.entity._
import collection.Seq

trait Environment{
  def atomic(f : => Any) : Any = {
    push()
    val ret = f
    pop()
    ret
  }

  /**
   * Выражение в Set, которое слева.
   * Это необходимо, чтобы выражение справа могло действовавать в контексте выражения слева.
   */
  def currentSet : Option[SetBase]
  def leftEntity : Option[Entity]

  def setSet(set : Option[SetBase], entity : Option[Entity])

  val cache : collection.mutable.Map[Any, Any] = collection.mutable.Map.empty[Any, Any]

  def start() {
    em.beginTransaction()
  }
  def end() {
    em.commit()
    cache.clear()
  }
  def em : EntityManager
  def model : ObjectModel

  def entityDescription(name : String, imports : Option[Imports] = None) = {
    model.entityDescriptionOption(name, imports).getOrElse{
       throw ScriptException("Entity %s not found".format(name))
    }
  }

  def dataSource(dataSourceName : Option[Expression], imports : Option[Imports] = None) : Option[DataSource] = dataSourceName match {
    case Some(e) => e.evaluate(this) match {
      case st : DataSource => Some(st)
      case name => Some(model.dataSource(name.toString, imports))
    }
    case None => currentDataSource
  }

  def currentDataSource : Option[DataSource]
  def setCurrentDataSource(dataSource : Option[DataSource])

  def thisType : Option[ScriptDataType]
  def setThisType(tc : Option[ScriptDataType])

  def dotType : Option[ScriptDataType]

  def setDotType(tc : Option[ScriptDataType])

  def refOption = if(dotRef.isDefined) dotRef else thisRef
  def ref = refOption.get

  def dotRef : Option[Any]

  def setDotRef(tc : Option[Any])

  def thisRef : Option[Any]

  def setThisRef(tc : Option[Any])

  def push()
  def pop()

  def addDeclaration(declaration : Declaration)
  def declarationsMap : collection.Map[String, Seq[Declaration]]
  def declarations(name : String) : Seq[Declaration] = declarationsMap(name)
  def declaration(name : String, parameters : Option[Seq[Par]] = None, imports : Option[Imports] = None) : DeclarationThis =
    declarationOption(name, parameters, imports).getOrElse{
      val tp = dotType.orElse{thisType}
      if(tp.isDefined) {
        throw ScriptException(
"""Could not find ref %s%s in "%s".
DataTypes: %s%s
Variants:
%s""".format(
          name, parameters.map("(" + _.mkString(", ") + ")").getOrElse(""), tp.get.toString,
          name, parameters.map(pars => "(" + pars.map(par => par.dataTypeString(this)).mkString(", ") + ")").getOrElse(""),
          tp.get.declarations.filter(_.name == name).map(_.declarationString).mkString("\n")
        ))
      }
      throw ScriptException("Could not find ref \"%s\"".format(name))
    }

  def declarationOption(name : String, parameters : Option[Seq[Par]] = None, imports : Option[Imports] = None) : Option[DeclarationThis] = {
    if(dotType.isDefined) {
      dotType.get.declaration(this, name, parameters)
    }
    else {
      if(name == "this") {
        Some(DeclarationThis(None, This(thisType.get)))
      } else {
        val declarations: Seq[Declaration] = declarationsMap.getOrElse(name, Seq())
        var ret : Option[DeclarationThis] = declarations.find(_.correspond(this, parameters)).map(r => DeclarationThis(None, r))
        if(!ret.isDefined) {
          ret = declarations.find(_.parameters.isEmpty).map{ declaration =>
            declaration.dataType(this, None).declaration(this, "apply", parameters) match {
              case Some(DeclarationThis(tt, dec, _)) => Some(DeclarationThis(tt, dec, Some(declaration)))
              case None => None
            }
          }.filter(_.isDefined).map(_.get)
          if(!ret.isDefined) {
            if(thisType.isDefined) {
              ret = thisType.get.declaration(this, name, parameters)
            }
            if(!ret.isDefined) {
              ret = globalDeclarationOption(name, parameters, imports)
            }
          }
        }
        ret
      }
    }
  }

  def globalDeclarationOption(name : String, parameters : Option[Seq[Par]] = None, imports : Option[Imports] = None) : Option[DeclarationThis]  = {
    var ret : Option[Declaration] = model.dataSourceOption(name, imports)
    var thisType : Option[ScriptDataType] = None
    if(!ret.isDefined) {
      ret = model.packOption(name, imports)
      if(!ret.isDefined) {
        ret = model.entityDescriptionOption(name, imports)
        if(!ret.isDefined) {
          ret = model.objOption(name, imports)
        }
        if(ret.isDefined) {
          if(parameters.isDefined) {
            thisType = Some(ret.get.dataType(this, None))
            ret = thisType.get.declaration(this, "apply", parameters).map(d=> d.declaration)
          }
        }
      }
    }
    ret.map{r => DeclarationThis(thisType, r)}
  }


  def values : collection.mutable.Map[Declaration, Any]
  def value(declaration : Declaration) = values(declaration)
  def update(declaration : Declaration, value : Any) : Any = {
    values.update(declaration, value)
    value
  }

  override def equals(obj: Any) = obj match {
    case env : Environment => env.declarationsMap == this.declarationsMap
    case _ => false
  }

  override def toString = "Environment(%s)".format(declarationsMap.toString())
}


class DefaultEnvironment(val model : ObjectModel = EntityConfiguration.model) extends Environment
{
  def push() {
    dataStack.push(data.save)
  }
  def pop() {
    data = dataStack.pop()
  }

  def addDeclaration(declaration : Declaration) {
    data.declarations.update(declaration.name, declaration +: data.declarations.getOrElseUpdate(declaration.name, Seq()) )
  }
  def declarationsMap = data.declarations
  def values = data.values


  var currentSet : Option[SetBase] = None
  var leftEntity : Option[Entity] = None
  def setSet(set: Option[SetBase], entity: Option[Entity]) {
    currentSet = set
    leftEntity = entity
  }

  var dotType : Option[ScriptDataType] = None
  def setDotType(tc: Option[ScriptDataType]) {
    dotType = tc
  }


  var thisType : Option[ScriptDataType] = None
  def setThisType(tc: Option[ScriptDataType]) {
    thisType = tc
  }

  var dotRef : Option[Any] = None
  def setDotRef(tc: Option[Any]) {
    dotRef = tc
  }

  var thisRef : Option[Any] = None
  def setThisRef(tc: Option[Any]) {
    thisRef = tc
  }

  var currentDataSource : Option[DataSource] = None
  def setCurrentDataSource(dataSource: Option[DataSource]) {
    currentDataSource = dataSource
  }

  protected var data : Data = new MutableData
  protected trait Data{
    def declarations : collection.mutable.Map[String, Seq[Declaration]]
    def values : collection.mutable.Map[Declaration, Any]
    def save = new SavedData(declarations.clone(), values)

    override def toString = declarations.toString()

    override def equals(obj: Any) = obj match {
      case d : Data => d.declarations == this.declarations && d.values == this.values
      case _ => false
    }
  }
  protected class MutableData extends Data{
    val declarations = collection.mutable.Map[String, Seq[Declaration]]()
    val values = collection.mutable.Map[Declaration, Any]()
  }
  protected class SavedData(val declarations : collection.mutable.Map[String, Seq[Declaration]],
                            val values : collection.mutable.Map[Declaration, Any]) extends Data

  protected val dataStack = Stack[Data]()

  protected var _em : EntityManager = null
  override def em = _em

  override def start() {
    _em = createEntityManager
    super.start()
  }

  protected def createEntityManager : EntityManager = new DefaultEntityManager

  override def end() {
    super.end()
    _em = null
  }
}