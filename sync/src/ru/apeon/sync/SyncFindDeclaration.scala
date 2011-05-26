package ru.apeon.sync

import ru.apeon.core.script._
import ru.apeon.core.entity.Entity

object SyncFindDeclaration extends Declaration{
  def value(env: Environment, parameters: Option[Seq[ParVal]], dataSource: Option[Expression]) = {
    val e = env.ref.asInstanceOf[Entity]
    Sync.syncFind(env, e, e.id.description, dataSource, None)
  }

  def name = "syncFind"

  def dataType(env: Environment, parameters: Option[Seq[Par]]) = ScriptDataTypeOption(env.dotType.get)
}