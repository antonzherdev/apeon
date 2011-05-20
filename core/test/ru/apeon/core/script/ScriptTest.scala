package ru.apeon.core.script

import ru.apeon.core.entity.EntityManager

/**
 * @author Anton Zherdev
 */

trait ScriptTest extends ScriptDefine{
  def model : ObjectModel
  def pack : Package

  def run(em : EntityManager, statement : Statement*) = Script(model, pack, statement : _*).evaluate(new Env(em))
}