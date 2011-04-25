package ru.apeon.core.script

object Script {
  def apply(pack : Package, statements : Statement*) = {
    new Script(pack, statements).preFillRef.fillRef()
  }

  def apply(pack : Package, model : ObjectModel, statements : Statement*) = {
    new Script(pack, statements).preFillRef.fillRef(new DefaultEnvironment(model, pack.dataSource))
  }

  def evaluate(pack : Package, statements : Seq[Statement]) : Any =
    evaluate(new DefaultEnvironment(pack.model, pack.dataSource), statements)

  def evaluate(env : Environment, statements : Seq[Statement]) : Any = {
    var ret : Any = null
    env.start()
    env.atomic{
      statements.foreach{statement =>
        ret = statement.evaluate(env)
      }
      ret
    }
    env.end()
    ret
  }
}

class Script(val pack : Package, val statements : Seq[Statement]){
  def fillRef(env : Environment = new DefaultEnvironment(pack.model, pack.dataSource)) : Script = {
    val imports = Imports(pack, statements.filter(_.isInstanceOf[Import]).map(_.asInstanceOf[Import].name))
    statements.foreach(_.fillRef(env, imports))
    this
  }

  def preFillRef = {
    val imports = Imports(pack, statements.filter(_.isInstanceOf[Import]).map(_.asInstanceOf[Import].name))
    statements.foreach(_.preFillRef(pack.model, imports))
    this
  }


  override def equals(obj: Any) = obj match {
    case s : Script => s.statements.corresponds(this.statements){_ == _}
    case _ => false
  }

  def evaluate(env : Environment = new DefaultEnvironment(pack.model, pack.dataSource)): Any =
    Script.evaluate(env, statements)

  override def toString = statements.map(_.toString).mkString("\n")
}

object ScriptException {
  def apply(env : Environment, message : String) = {
    new ScriptException("%s\nStack:\n%s".format(message, env.stackString))
  }
}

class ScriptException(message : String) extends RuntimeException(message)