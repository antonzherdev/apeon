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
  def correspond(env : Environment, parameters : Option[Seq[Par]]) : Boolean

  /**
   * Вернуть типы параметров для встроенной функции
   * @param env окружение
   * @param parameterNumber номер параметра
   * @param parameter параметер
   */
  def builtInParametersDataTypes(env : Environment, parameterNumber : Int, parameter : Par) : Seq[ScriptDataType] =
    throw ScriptException(env, "Not supported")
}

trait DeclarationStatement extends Statement with Declaration {
  def preFillRef(model: ObjectModel, imports: Imports) {}
  def fillRef(env: Environment, imports: Imports) {}
  def evaluate(env: Environment) {}
  def dataType(env: Environment, parameters: Option[Seq[Par]]) = dataType(env)
}

/**
 * Ссылка на задекларированную переменную
 */
case class Ref(name : String, parameters : Option[Seq[Par]] = None, dataSource : Option[Expression] = None) extends Expression {
  private var _declaration : Declaration = _

  def declaration : Declaration = _declaration

  def dataType(env : Environment) = _declaration.dataType(env, parameters)

  def evaluate(env: Environment) =
    _declaration.value(env,
      env.withDotRef(None) {
        parameters.map(_.map(par => ParVal(par.expression.evaluate(env), par.name)))
      },
      dataSource)


  def fillRef(env : Environment, imports : Imports) {
    if(dataSource.isDefined) {
      env.withDotType(None) {
        dataSource.get.fillRef(env, imports)
      }
    }
    if(parameters.isDefined && (!parameters.get.isEmpty)) {
      env.withDotType(None) {
        parameters.foreach(_.foreach(_.expression.fillRef(env, imports)))
      }
    }
    _declaration = env.declaration(name, parameters, Some(imports))
    if(parameters.isDefined) {
      var number = 0
      parameters.get.foreach{par =>
        if(par.expression.isInstanceOf[BuiltInFunction]) {
          env.withDotType(None) {
            par.expression.asInstanceOf[BuiltInFunction].fillRef(env, imports, _declaration.builtInParametersDataTypes(env, number, par))
          }
        }
        number += 1
      }
    }
  }

  def preFillRef(model: ObjectModel, imports: Imports) {
    if(dataSource.isDefined) {
      dataSource.get.preFillRef(model, imports)
    }
    parameters.foreach(_.foreach(_.expression.preFillRef(model, imports)))
  }
}

case class Par(expression : Expression, name : Option[String] = None)

case class BuiltInFunction(statement : Statement, aliases : Seq[String] = Seq()) extends Constant {
  def dataType(env: Environment) = ScriptDataTypeBuiltInFunction()

  def value = this

  private var parameters : Seq[ParDeclaration] = _
  def fillRef(env: Environment, imports: Imports, parametersTypes : Seq[ScriptDataType]) = {
    val iAliases = aliases.iterator
    parameters = parametersTypes.map(tp =>
      ParDeclaration(if(iAliases.hasNext) iAliases.next() else "_", tp)
    )
    env.atomic{
      parameters.foreach(env.addDeclaration(_))
      statement.fillRef(env, imports)
    }
  }

  override def preFillRef(model: ObjectModel, imports: Imports) {
    statement.preFillRef(model, imports)
  }

  case class ParDeclaration(name : String, dataType : ScriptDataType) extends  Declaration{
    def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = env.value(this)
    def dataType(env: Environment, parameters : Option[Seq[Par]]) = dataType
    def correspond(env: Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
  }

  def parameter(number : Int) = parameters(number)

  def run(env: Environment, parameterValues : Any*) : Any = env.atomic{
    val iValues = parameterValues.iterator
    parameters.foreach{parameter =>
      env.addDeclaration(parameter)
      env.update(parameter, iValues.next())
    }
    statement.evaluate(env)
  }
}


/**
 * Объявление функции
 * @param columnName имя функции
 * @param parameters список параметров
 * @param resultType возвращаемый тип, если явно указан
 * @param statement выражение
 */
case class Def(name : String, statement : Statement, parameters : Seq[DefPar] = Seq(),
               resultType : Option[ScriptDataType] = None)
        extends DeclarationStatement
{
  def dataType(env : Environment) = resultType match {
    case Some(ret) => ret
    case None => statement.dataType(env)
  }

  override def evaluate(env: Environment) {
    env.addDeclaration(this)
  }

  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource : Option[Expression]) = env.atomic{
    if(parameters.isDefined) {
      val i = parameters.get.iterator

      this.parameters.foreach{parameter =>
        env.addDeclaration(parameter)
        env.update(parameter, i.next().value)
      }
    }
    env.withThisRef(env.dotRef) {
      env.withDotRef(None) {
        if(dataSource.isDefined) {
          env.withDataSource(env.dataSource(dataSource)) {
            statement.evaluate(env)
          }
        } else {
          statement.evaluate(env)
        }
      }
    }
  }

  def correspond(env : Environment, parameters: Option[Seq[Par]]) = parameters match {
    case None => this.parameters.isEmpty
    case _ => parameters.get.corresponds(this.parameters){(p : Par, dp : DefPar) => dp.dataType == p.expression.dataType(env)}
  }


  override def fillRef(env: Environment, imports: Imports) {
    env.addDeclaration(this)
    env.atomic{
      this.parameters.foreach{parameter =>
        env.addDeclaration(parameter)
      }
      statement.fillRef(env, imports)
    }
    if(resultType.isDefined) resultType.get.fillRef(env, imports)
  }

  override def preFillRef(model: ObjectModel, imports: Imports) {
    statement.preFillRef(model, imports)
    if(resultType.isDefined) resultType.get.preFillRef(model, imports)
  }
}

case class ParVal(value : Any, name : Option[String])

/**
 * Объявление параметра функции
 */
case class DefPar(name : String, dataType : ScriptDataType) extends Declaration {
  def dataType(env: Environment, parameters : Option[Seq[Par]]) = dataType

  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource : Option[Expression]) = env.value(this)

  def correspond(env : Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty
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

  def correspond(env : Environment, parameters: Option[Seq[Par]]) = parameters.isEmpty

  def init : Expression

  override def fillRef(env: Environment, imports: Imports) {
    env.addDeclaration(this)
    init.fillRef(env, imports)
  }

  override def preFillRef(model: ObjectModel, imports: Imports) {
    init.preFillRef(model, imports)
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

