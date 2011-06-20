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
        statements.foreach{stm =>
          try{
            ret = stm.evaluate(env)
          } catch thrCatch(stm)
        }
        ret
      }
    }
    finally {
      env.end()
    }
    ret
  }

  def thrCatch(stm : Statement) : PartialFunction[Throwable, Any] = {
    case s @ ScriptException(message, cause, None, file) =>
      throw ScriptException(message, cause.orElse(Some(s)), Some(stm), file)
    case t => throw ScriptException(t.getMessage, Some(t), Some(stm))
  }
}

class Script(val model : ObjectModel, val pack : Package, val statements : Seq[Statement], val fileName : Option[String] = None){
  def fillRef(env : Environment = new DefaultEnvironment(model)) : Script = {
    val imports = Imports(pack, statements.filter(_.isInstanceOf[Import]).map(_.asInstanceOf[Import].name))
    statements.foreach{stm =>
      try{
        try{
          stm.fillRef(env, imports)
        } catch Script.thrCatch(stm)
      }catch tt
    }
    this
  }

  private def tt : PartialFunction[Throwable, Script] = {
    case s @ ScriptException(message, cause, stm, _) if fileName.isDefined =>
      throw ScriptException(message, cause.orElse{Some(s)}, stm, fileName)
    case s : Throwable if fileName.isDefined =>
      throw ScriptException(s.getMessage, Some(s), None, fileName)
  }

  def preFillRef(env : Environment = new DefaultEnvironment(model)) : Script =  {
    val imports = Imports(pack, statements.filter(_.isInstanceOf[Import]).map(_.asInstanceOf[Import].name))
    statements.foreach(stm =>
      try{
        try{
          stm.preFillRef(env, imports)
        } catch Script.thrCatch(stm)
      }catch tt
    )
    this
  }

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
                           near : Option[Statement] = None,
                           file : Option[String] = None) extends RuntimeException(message, cause.getOrElse{null}) {
  override def getMessage = (near, file) match {
    case (None, None) => message
    case (Some(n), None) => "%s near %s".format(message, n)
    case (Some(n), Some(f)) => "%s near %s in file %s".format(message, n, f)
    case (None, Some(f)) => "%s in file %s".format(message, f)
  }
}