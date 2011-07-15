package ru.apeon.core.script

import ru.apeon.core.entity.{EntityDefine, Env, EntityManager}

trait ScriptTest extends ScriptDefine with EntityDefine{
  def run(em : EntityManager, statement : Statement*) = Script(model, pack, statement : _*).evaluate(new Env(em))
  def run(statement : Statement*) = Script(model, pack, statement : _*).evaluate(new DefaultEnvironment())

  def parse(s : String) : Script = ScriptParser.parse(model, CoreModule, pack, s)
  def script(statement : Statement*) = new Script(model, pack, statement.toSeq)
}