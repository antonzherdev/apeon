package ru.apeon.core.script

object Script {
  def apply(model : ObjectModel, pack : Package, statements : Statement*) = {
    new Script(model, pack, statements).preFillRef().fillRef()
  }

  def evaluate(model : ObjectModel, statements : Seq[Statement]) : Any =
    evaluate(new DefaultEnvironment(model), statements)

  def evaluate(env : Environment, statements : Seq[Statement]) : Any = {
    var ret : Any = null
    env.start()
    try{
      env.atomic{
        statements.foreach{statement =>
          ret = statement.evaluate(env)
        }
        ret
      }
    }
    finally {
      env.end()
    }
    ret
  }
}

class Script(val model : ObjectModel, val pack : Package, val statements : Seq[Statement], val fileName : Option[String] = None){
  def fillRef(env : Environment = new DefaultEnvironment(model)) : Script = try{
    val imports = Imports(pack, statements.filter(_.isInstanceOf[Import]).map(_.asInstanceOf[Import].name))
    statements.foreach(stm => stm.fillRef(env, imports))
    this
  } catch tt

  def tt : PartialFunction[Throwable, Script] = {
    case s : Throwable if fileName.isDefined =>
      throw new ScriptException("%s in file %s".format(s.getMessage, fileName.get), Some(s))
  }

  def preFillRef(env : Environment = new DefaultEnvironment(model)) : Script = try {
    val imports = Imports(pack, statements.filter(_.isInstanceOf[Import]).map(_.asInstanceOf[Import].name))
    statements.foreach(stm => stm.preFillRef(env, imports))
    this
  } catch tt

  override def equals(obj: Any) = obj match {
    case s : Script => s.statements.corresponds(this.statements){_ == _}
    case _ => false
  }

  def evaluate(env : Environment = new DefaultEnvironment(model)): Any = try {
    Script.evaluate(env, statements)
  } catch tt

  override def toString = statements.map(_.toString).mkString("\n")
}


case class ScriptException(message : String,
                           cause: Option[Throwable] = None,
                           near : Option[Statement] = None) extends RuntimeException(message, cause.getOrElse{null}) {
  override def getMessage = near match {
    case None => message
    case Some(n) => "%s near %s".format(message, n)
  }
}