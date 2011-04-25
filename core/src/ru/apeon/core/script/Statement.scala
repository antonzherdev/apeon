package ru.apeon.core.script

import ru.apeon.core._
import entity._
import collection.mutable.Buffer


/**
 * Оператор
 */
trait Statement {
  def dataType(env : Environment) : ScriptDataType

  def evaluate(env : Environment) : Any

  def preFillRef(model : ObjectModel, imports : Imports)

  /**
   * Заполнение ссылок
   * @env окружение
   * @imports импортировано
   */
  def fillRef(env : Environment, imports : Imports)
}

abstract class StatementList extends Statement {
  def statements : Seq[Statement]

  def dataType(env : Environment) = statements match {
    case Seq() => ScriptDataTypeUnit()
    case _ => statements.last.dataType(env)
  }

  def evaluate(env: Environment) = env.atomic{
    var ret : Any = null
    statements.foreach{statement =>
      ret = statement.evaluate(env)
    }
    ret
  }

  def preFillRef(model: ObjectModel, imports: Imports) {
    statements.foreach(_.preFillRef(model, imports))
  }

  def fillRef(env : Environment, imports : Imports) {
    env.atomic {
      statements.foreach(_.fillRef(env, imports))
    }
  }
}

/**
 * Фигурные скобочки.
 * Список опираторов
 */
case class Parentheses(statements : Seq[Statement] = Seq()) extends StatementList


/**
 * Выражение Eql
 * @param выражение в строковом виде
 */
case class Eql(string : String) extends Expression {
  private var stm : eql.Statement = _
  private val externals = Buffer[EqlExternalScript]()

  def dataType(env : Environment) = stm match {
    case select : eql.Select => select.columns match {
      case Seq() => select.from match {
        case e : eql.FromEntity => ScriptDataTypeSeq(ScriptDataTypeEntityByDescription(e.entity))
        case _ => ScriptDataTypeSeq(ScriptDataTypeSeq(ScriptDataTypeAny()))
      }
      case _ => ScriptDataTypeSeq(ScriptDataTypeSeq(ScriptDataTypeAny()))
    }
    case _ => ScriptDataTypeUnit()
  }

  def evaluate(env: Environment) = {
    externals.foreach(_.evaluate(env))
    stm match {
      case select : eql.Select => dataType(env) match {
        case ScriptDataTypeSeq(ScriptDataTypeEntity(entity)) => env.em.select(select)
        case _ => select.dataSource.store.select(select)
      }
      case update : eql.Update => update.dataSource.store.update(update)
      case insert : eql.Insert => insert.dataSource.store.insert(insert)
      case delete : eql.Delete => delete.dataSource.store.delete(delete)
    }
  }


  def preFillRef(model: ObjectModel, imports: Imports) {
     stm = eql.EqlParser(string, model, Some(imports),Some({s : String =>
      val external = new EqlExternalScript(new ScriptParser(model).parseStatement(s))
      external.preFillRef(model, imports)
      externals.append(external)
      external
    }))
  }

  def fillRef(env : Environment, imports : Imports) {
    externals.foreach(_.fillRef(env, imports))
  }
}

class EqlExternalScript(val statement : Statement) extends eql.External {
  private var _data : Any = _
  def data = _data

  def fillRef(env : Environment, imports : Imports) {
    statement.fillRef(env, imports)
  }

  def preFillRef(model : ObjectModel, imports : Imports) {
    statement.preFillRef(model, imports)
  }

  def evaluate(env: Environment) {
    _data = statement.evaluate(env)
  }

  def eqlExpression = eql.Const(_data)
}

/**
 * Доступ к колонке сущности через точку
 */
case class Dot(left : Expression, right : Ref) extends Expression{
  def dataType(env : Environment) = right.dataType(env)

  def evaluate(env: Environment) = env.withDotRef(Some(left.evaluate(env))){
    right.evaluate(env)
  }

  def fillRef(env : Environment, imports : Imports) {
    left.fillRef(env, imports)
    env.withDotType(Some(left.dataType(env))) {
      right.fillRef(env, imports)
    }
  }

  def preFillRef(model: ObjectModel, imports: Imports) {
    left.preFillRef(model, imports)
    right.preFillRef(model, imports)
  }
}

abstract class SetBase extends Expression {
  def left : Expression
  def right : Expression
  def name : String

  def evaluate(env: Environment) = left match {
    case ref @ Ref(name, None, None) => env.withSet(Some(this), None){
      evaluate(env, ref)
    }
    case Dot(expr, ref) => expr.dataType(env) match {
      case ScriptDataTypeEntity(entity) => expr.evaluate(env) match {
        case e : Entity => {
          env.withSet(Some(this), Some(e)) {
            evaluate(env, e, ref)
          }
        }
        case _ => throw ScriptException(env, "Return is not entity")
      }
      case _ => throw ScriptException(env, "Unsupported datatype for dot in set")
    }
    case _ => throw ScriptException(env, "Unsupported expression for set")
  }

  def evaluate(env: Environment, ref : Ref) : Any
  def evaluate(env: Environment, entity : Entity, ref : Ref)  : Any

  def dataType(env: Environment) = left.dataType(env)

  def fillRef(env : Environment, imports : Imports) {
    left.fillRef(env, imports)
    right.fillRef(env, imports)
  }
  def preFillRef(model: ObjectModel, imports: Imports) {
    left.preFillRef(model, imports)
    right.preFillRef(model, imports)
  }
  override def toString = "%s %s %s".format(left, name, right)
}

case class Set(left : Expression, right : Expression) extends SetBase {
  def evaluate(env: Environment, ref: Ref) = env.update(ref.declaration, right.evaluate(env))

  def evaluate(env: Environment, entity: Entity, ref: Ref) = {
    val ret = right.evaluate(env)
    entity.update(ref.name, ret)
    ret
  }
  def name = "="
}

case class SetPlus(left : Expression, right : Expression) extends SetBase {
  def evaluate(env: Environment, ref: Ref) =
    env.update(ref.declaration, Plus.evaluate(env, ref.declaration.value(env), right.evaluate(env)))

  def evaluate(env: Environment, entity: Entity, ref: Ref) = {
    val ret = Plus.evaluate(env, entity(ref.name), right.evaluate(env))
    entity.update(ref.name, ret)
    ret
  }
  def name = "+="
}

case class SetMinus(left : Expression, right : Expression) extends SetBase {
  def evaluate(env: Environment, ref: Ref) =
    env.update(ref.declaration, Minus.evaluate(env, ref.declaration.value(env), right.evaluate(env)))

  def evaluate(env: Environment, entity: Entity, ref: Ref) = {
    val ret = Minus.evaluate(env, entity(ref.name), right.evaluate(env))
    entity.update(ref.name, ret)
    ret
  }
  def name = "-="
}

case class SetMul(left : Expression, right : Expression) extends SetBase {
  def evaluate(env: Environment, ref: Ref) =
    env.update(ref.declaration, Mul.evaluate(env, ref.declaration.value(env), right.evaluate(env)))

  def evaluate(env: Environment, entity: Entity, ref: Ref) = {
    val ret = Mul.evaluate(env, entity(ref.name), right.evaluate(env))
    entity.update(ref.name, ret)
    ret
  }
  def name = "*="
}

case class SetDiv(left : Expression, right : Expression) extends SetBase {
  def evaluate(env: Environment, ref: Ref) =
    env.update(ref.declaration, Div.evaluate(env, ref.declaration.value(env), right.evaluate(env)))

  def evaluate(env: Environment, entity: Entity, ref: Ref) = {
    val ret = Div.evaluate(env, entity(ref.name), right.evaluate(env))
    entity.update(ref.name, ret)
    ret
  }
  def name = "/="
}


case class If(check : Expression, forTrue : Statement, forFalse : Option[Statement] = None) extends Expression {
  def dataType(env: Environment) = forTrue.dataType(env)

  def evaluate(env: Environment) = {
    val c : Boolean = check.evaluate(env) match {
      case b : Boolean => b
      case _ => throw ScriptException(env, "Unsupported datatype for if")
    }
    if(c) forTrue.evaluate(env)
    else {
      if(forFalse.isDefined)forFalse.get.evaluate(env)
      else null
    }
  }

  def fillRef(env : Environment, imports : Imports) {
    check.fillRef(env, imports)
    forTrue.fillRef(env, imports)
    if(forFalse.isDefined) forFalse.get.fillRef(env, imports)
  }

  def preFillRef(model: ObjectModel, imports: Imports) {
    check.preFillRef(model, imports)
    forTrue.preFillRef(model, imports)
    if(forFalse.isDefined) forFalse.get.preFillRef(model, imports)
  }
}

case class Import(name : String) extends Statement {
  def evaluate(env: Environment) = null

  def dataType(env: Environment) = ScriptDataTypeUnit()

  def fillRef(env : Environment, imports : Imports) {}

  def preFillRef(model: ObjectModel, imports: Imports) {}
}