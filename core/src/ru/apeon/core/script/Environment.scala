package ru.apeon.core.script

import collection.mutable.Stack
import ru.apeon.core.entity._

trait Environment{
  def fileName : Option[String]

  def atomic(f : => Any) : Any = {
    push()
    val ret = f
    pop()
    ret
  }

  val stack = Stack[Statement]()

  def evaluate(statement : Statement) : Any ={
    stack.push(statement)
    try {
      statement.evaluate(this)
    }
    finally {
      stack.pop()
    }
  }

  def fillRef(statement : Statement, imports : Imports) : Any ={
    stack.push(statement)
    try {
      statement.fillRef(this, imports)
    }
    finally {
      stack.pop()
    }
  }

  def preFillRef(statement : Statement, imports : Imports) : Any ={
    stack.push(statement)
    try {
      statement.preFillRef(this, imports)
    }
    finally {
      stack.pop()
    }
  }

  def stackString = stack.mkString("\n")

  /**
   * Выражение в Set, которое слева.
   * Это необходимо, чтобы выражение справа могло действовавать в контексте выражения слева.
   */
  def currentSet : Option[SetBase]
  def leftEntity : Option[Entity]
  def withSet[A](set : Option[SetBase], entity : Option[Entity])(f : => A) = {
    val oldSet = currentSet
    val oldEntity = leftEntity
    setSet(set, entity)
    val ret = f
    setSet(oldSet, oldEntity)
    ret
  }
  protected def setSet(set : Option[SetBase], entity : Option[Entity])

  def start() {
    em.beginTransaction()
  }
  def end() {
    em.commit()
  }
  def em : EntityManager
  def model : ObjectModel

  def entityDescription(name : String, imports : Option[Imports] = None) = {
    model.entityDescriptionOption(name, imports).getOrElse{
       throw ScriptException(this, "Entity %s not found".format(name))
    }
  }

  def dataSource(dataSourceName : Option[Expression], imports : Option[Imports] = None) : Option[DataSource] = dataSourceName match {
    case Some(e) => e.evaluate(this) match {
      case st : DataSource => Some(st)
      case name => Some(model.dataSource(name.toString, imports))
    }
    case None => currentDataSource
  }
  def withDataSource[A](dataSource : Option[DataSource])( f : => A) : A = {
    val old = this.currentDataSource
    setCurrentDataSource(dataSource)
    val ret = f
    setCurrentDataSource(old)
    ret
  }
  protected def currentDataSource : Option[DataSource]
  protected def setCurrentDataSource(dataSource : Option[DataSource])

  def thisType : Option[ScriptDataType]
  protected def setThisType(tc : Option[ScriptDataType])
  def withThisType[A](tc : ScriptDataType)( f : => A) : A = {
    val old = thisType
    setThisType(Some(tc))
    val ret = f
    setThisType(old)
    ret
  }

  def dotType : Option[ScriptDataType]
  def withDotType[A](tc : Option[ScriptDataType])( f : => A) : A = {
    val old = dotType
    setDotType(tc)
    val ret = f
    setDotType(old)
    ret
  }
  protected def setDotType(tc : Option[ScriptDataType])

  def refOption = if(dotRef.isDefined) dotRef else thisRef
  def ref = refOption.get

  def dotRef : Option[Any]
  def withDotRef[A](tc : Option[Any])(f : => A) : A = {
    val old = dotRef
    setDotRef(tc)
    val ret = f
    setDotRef(old)
    ret
  }
  protected def setDotRef(tc : Option[Any])

  def thisRef : Option[Any]
  def withThisRef[A](tc : Option[Any])(f : => A) : A = {
    val old = thisRef
    setThisRef(tc)
    val ret = f
    setThisRef(old)
    ret
  }
  protected def setThisRef(tc : Option[Any])

  def push()
  def pop()

  def addDeclaration(declaration : Declaration)
  def declarationsMap : collection.Map[String, Seq[Declaration]]
  def declarations(name : String) : Seq[Declaration] = declarationsMap(name)
  def declaration(name : String, parameters : Option[Seq[Par]] = None, imports : Option[Imports] = None) : DeclarationThis =
    declarationOption(name, parameters, imports).getOrElse{
      if(dotType.isDefined) {
        throw ScriptException(this,
"""Could not find ref %s%s in "%s".
DataTypes: %s%s
Variants:
%s""".format(
          name, parameters.map("(" + _.mkString(", ") + ")").getOrElse(""), dotType.get.toString,
          name, parameters.map(pars => "(" + pars.map(par => par.dataTypeString(this)).mkString(", ") + ")").getOrElse(""),
          dotType.get.declarations.filter(_.name == name).map(_.declarationString).mkString("\n")
        ))
      }
      throw ScriptException(this, "Could not find ref \"%s\"".format(name))
    }

  def declarationOption(name : String, parameters : Option[Seq[Par]] = None, imports : Option[Imports] = None) : Option[DeclarationThis] = {
    if(dotType.isDefined) {
      dotType.get.declaration(this, name, parameters)
    }
    else {
      if(name == "this") {
        Some(DeclarationThis(None, This(thisType.get)))
      } else {
        var ret : Option[DeclarationThis] = declarationsMap.getOrElse(name,Seq()).find(_.correspond(this, parameters)).map(r => DeclarationThis(None, r))
        if(!ret.isDefined) {
          if(thisType.isDefined) {
            ret = thisType.get.declaration(this, name, parameters)
          }
          if(!ret.isDefined) {
            ret = globalDeclarationOption(name, parameters, imports)
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


class DefaultEnvironment(val model : ObjectModel = EntityConfiguration.model, val fileName : Option[String] = None) extends Environment
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
  protected def setThisType(tc: Option[ScriptDataType]) {
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
  protected def setCurrentDataSource(dataSource: Option[DataSource]) {
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
  }

  protected def createEntityManager : EntityManager = new DefaultEntityManager

  override def end() {
    super.end()
    _em = null
  }
}