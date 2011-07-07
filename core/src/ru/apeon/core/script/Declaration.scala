package ru.apeon.core.script

/**
 * Декларация переменной
 */
trait Declaration{
  /**
   * Название
   */
  def name : String

  /**
   * Тип данных
   * @param env окружение
   * @param parameters параметры
   */
  def dataType(env : Environment, parameters : Option[Seq[Par]]) : ScriptDataType

  /**
   * Получить значение
   * @param env окружение
   * @param parameters параметры
   */
  def value(env : Environment, parameters : Option[Seq[ParVal]] = None, dataSource : Option[Expression] = None) : Any

  /**
   * Соответствует ли набор параметров тому, что можно выполнить
   * @param parameters параметры
   */
  def correspond(env : Environment, parameters : Option[Seq[Par]]) : Boolean = parameters match {
    case None => this.parameters.isEmpty
    case _ => parameters.get.corresponds(this.parameters){(p : Par, dp : DefPar) =>
      dp.dataType.correspond(p.expression.dataType(env))
    }
  }

  /**
   * Вернуть типы параметров для встроенной функции
   * @param env окружение
   * @param parametes параметры
   * @param parameterNumber номер параметра
   * @param parameter параметер
   */
  def builtInParameters(env : Environment, parameters: Option[Seq[Par]], parameterNumber : Int, parameter : Par) : Seq[BuiltInParameterDef] =
    throw ScriptException( "Not supported")

  def declarationString : String = name + "(" + parameters.mkString(", ") + ")"

  def equalsSignature(declaration : Declaration) : Boolean =
    this.name == declaration.name && this.parameters.sameElements(declaration.parameters)

  def parameters : Seq[DefPar] = Seq()
}

case class BuiltInParameterDef(defaultName : String, dataType : ScriptDataType)

trait DeclarationStatement extends Statement with Declaration {
  def preFillRef(env: Environment, imports: Imports) {}
  def fillRef(env: Environment, imports: Imports) {}
  def evaluate(env: Environment) {}
  def dataType(env: Environment, parameters: Option[Seq[Par]]) = dataType(env)
}

case class DeclarationThis(thisType : Option[ScriptDataType], declaration : Declaration, thisDeclaration : Option[Declaration] = None)

/**
 * Ссылка на задекларированную переменную
 */
case class Ref(name : String, parameters : Option[Seq[Par]] = None, dataSource : Option[Expression] = None) extends Expression {
  private var _declaration : DeclarationThis = _
  def declaration : Declaration = _declaration.declaration

  def dataType(env : Environment) = {
    val old = env.dotType
    env.setDotType(_declaration.thisType)
    val ret = declaration.dataType(env, parameters)
    env.setDotType(old)
    ret
  }

  def evaluate(env: Environment) = {
    val old = env.dotType
    env.setDotType(_declaration.thisType)
    try {
      val oldRef = env.dotRef
      env.setDotRef(None)
      val pars = parameters.map(_.map(par => ParVal(par.expression.evaluate(env), par.name)))
      env.setDotRef(oldRef)

      if(_declaration.thisDeclaration.isDefined) {
        env.setDotRef(Some(_declaration.thisDeclaration.get.value(env)))
        val ret = declaration.value(env, pars, dataSource)
        env.setDotRef(oldRef)
        ret
      } else {
        declaration.value(env, pars, dataSource)
      }
    }
    finally {
      env.setDotType(old)
    }
  }


  def fillRef(env : Environment, imports : Imports) {
    if(dataSource.isDefined) {
      val old = env.dotType
      env.setDotType(None)
      dataSource.get.fillRef(env, imports)
      env.setDotType(old)
    }
    if(parameters.isDefined && (!parameters.get.isEmpty)) {
      val old = env.dotType
      env.setDotType(None)
      parameters.foreach(_.foreach(par => par.expression.fillRef(env, imports)))
      env.setDotType(old)
    }
    _declaration = env.declaration(name, parameters, Some(imports))
    if(parameters.isDefined) {
      var number = 0
      parameters.get.foreach{par =>
        if(par.expression.isInstanceOf[BuiltInFunction]) {
          val old = env.dotType
          env.setDotType(_declaration.thisType)
          val bps = declaration.builtInParameters(env, parameters, number, par)
          env.setDotType(None)
          par.expression.asInstanceOf[BuiltInFunction].fillRef(env, imports, bps)
          env.setDotType(old)
        }
        number += 1
      }
    }
  }

  def preFillRef(env : Environment, imports: Imports) {
    if(dataSource.isDefined) {
      dataSource.get.preFillRef(env, imports)
    }
    parameters.foreach(_.foreach(par => par.expression.preFillRef(env, imports)))
  }

  override def toString = name +
          dataSource.map(exp => "<%s>".format(exp)).getOrElse("") +
          parameters.map(pars => "(%s)".format(pars.mkString(", "))).getOrElse("")
}

case class Par(expression : Expression, name : Option[String] = None) {
  override def toString = name match {
    case Some(nm) => "%s = %s".format(nm, expression)
    case None => expression.toString
  }

  def dataTypeString(env : Environment) : String = name match {
    case Some(nm) => "%s = %s".format(nm, expression.dataType(env))
    case None => expression.dataType(env).toString
  }
}

case class BuiltInFunction(statement : StatementList, aliases : Seq[String] = Seq()) extends Constant {
  def dataType(env: Environment) = ScriptDataTypeBuiltInFunction()

  def value = this

  private var _parameters : Seq[ParDeclaration] = _
  def fillRef(env: Environment, imports: Imports, parametersTypes : Seq[BuiltInParameterDef]) = {
    val iAliases = aliases.iterator
    _parameters = parametersTypes.map(tp =>
      ParDeclaration(if(iAliases.hasNext) iAliases.next() else tp.defaultName, tp.dataType)
    )
    env.atomic{
      _parameters.foreach(env.addDeclaration(_))
      statement.fillRef(env, imports)
    }
  }

  override def preFillRef(env : Environment, imports: Imports) {
    statement.preFillRef(env, imports)
  }

  case class ParDeclaration(name : String, dataType : ScriptDataType) extends  Declaration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = env.value(this)
    def dataType(env: Environment, parameters : Option[Seq[Par]]) = dataType
  }

  def parameters : Seq[ParDeclaration] = _parameters

  def run(env: Environment, parameterValues : Any*) : Any = env.atomic{
    val iValues = parameterValues.iterator
    _parameters.foreach{parameter =>
      env.addDeclaration(parameter)
      env.update(parameter, iValues.next())
    }
    statement.evaluate(env)
  }

  override def toString = "%s =>\n%s".format(aliases.mkString(","), statement)
}


/**
 * Объявление функции
 * @param columnName имя функции
 * @param parameters список параметров
 * @param resultType возвращаемый тип, если явно указан
 * @param statement выражение
 */
case class Def(name : String, statement : Statement, override val parameters : Seq[DefPar] = Seq(),
               resultType : Option[ScriptDataType] = None, cached : Boolean = false)
        extends DeclarationStatement
{
  def dataType(env : Environment) = resultType match {
    case Some(ret) => ret
    case None => statement.dataType(env)
  }

  override def evaluate(env: Environment) {
    env.addDeclaration(this)
  }

  private def eval(env: Environment, parameters: Option[scala.Seq[ParVal]], dataSource: Option[Expression]): Any =
    try {
      env.atomic{
        if (parameters.isDefined) {
          val i = parameters.get.iterator

          this.parameters.foreach {
            parameter =>
              env.addDeclaration(parameter)
              env.update(parameter, i.next().value)
          }
        }
        val oldThisRef = env.thisRef
        env.setThisRef(env.dotRef)
        val oldDotRef = env.dotRef
        env.setDotRef(None)
        try{
          if (dataSource.isDefined) {
            val oldDS = env.currentDataSource
            env.setCurrentDataSource(env.dataSource(dataSource))
            val ret = statement.evaluate(env)
            env.setCurrentDataSource(oldDS)
            ret
          } else {
            statement.evaluate(env)
          }
        } finally {
          env.setThisRef(oldThisRef)
          env.setDotRef(oldDotRef)
        }
      }
    }
    catch Script.thrCatch(this)

  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource : Option[Expression]) =
    if(cached){
      env.cache.getOrElseUpdate((this, parameters.getOrElse{Seq()}.map{_.value}).hashCode(),
        eval(env, parameters, dataSource))
    } else {
      eval(env, parameters, dataSource)
    }

  override def fillRef(env: Environment, imports: Imports) {
    try {
      env.addDeclaration(this)
      env.atomic{
        this.parameters.foreach{parameter =>
          env.addDeclaration(parameter)
        }
        statement.fillRef(env, imports)
      }
      if(resultType.isDefined) resultType.get.fillRef(env, imports)
    }
    catch Script.thrCatch(this)
  }

  override def preFillRef(env : Environment, imports: Imports) {
    try {
      statement.preFillRef(env, imports)
      if(resultType.isDefined) resultType.get.preFillRef(env, imports)
      parameters.foreach{parameter =>
        parameter.dataType.preFillRef(env, imports)
      }
    }
    catch Script.thrCatch(this)
  }

  override def declarationString = name + "(" + parameters.mkString(", ") + ")" + resultType.map(" : " + _).getOrElse("")

  override def toString = declarationString + " = " + statement
}

case class ParVal(value : Any, name : Option[String])

/**
 * Объявление параметра функции
 */
case class DefPar(name : String, dataType : ScriptDataType) extends Declaration {
  def dataType(env: Environment, parameters : Option[Seq[Par]]) = dataType
  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource : Option[Expression]) = env.value(this)
  override def toString = "%s : %s".format(name, dataType)
}

abstract class VariableDeclaration extends DeclarationStatement {
  override def evaluate(env: Environment) {
    env.addDeclaration(this)
    env.update(this, init.evaluate(env))
  }

  def name : String

  def dataType : Option[ScriptDataType]

  def dataType(env: Environment) = dataType match {
    case Some(ret) => ret
    case None => init.dataType(env)
  }
  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource : Option[Expression]) = env.value(this)
  def init : Expression

  override def fillRef(env: Environment, imports: Imports) {
    env.addDeclaration(this)
    init.fillRef(env, imports)
  }

  override def preFillRef(env : Environment, imports: Imports) {
    init.preFillRef(env, imports)
  }
}

/**
 * Объявление неизменяемой переменной
 * @param columnName имя
 * @param init выражение для значения
 * @param dataType тип данных, если указан
 */
case class Val(name : String, init : Expression, dataType : Option[ScriptDataType] = None) extends VariableDeclaration

/**
 * Объявление изменяемой переменной
 * @param columnName имя
 * @param init инициализирующее значение
 * @param dataType тип данных, если указан
 */
case class Var(name : String, init : Expression, dataType : Option[ScriptDataType] = None) extends VariableDeclaration

case class This(thisDataType : ScriptDataType) extends Declaration {
  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = env.thisRef.get
  def name = "this"
  def dataType(env: Environment, parameters: Option[Seq[Par]]) = thisDataType
}