package ru.apeon.sync

case class SyncOptions(sync : SyncMode = InsertUpdate(), auto : AutoMode = AutoUpdate())

abstract class SyncMode
case class InsertOnly() extends SyncMode
case class InsertUpdate() extends SyncMode

abstract class AutoMode
case class NoAutoUpdate() extends AutoMode
case class AutoUpdate(one : AutoToOneMode = AutoToOne(), many : AutoToManyMode = AutoToManySet()) extends AutoMode


abstract class AutoToOneMode
case class NoAutoToOne() extends AutoToOneMode
case class AutoToOne() extends AutoToOneMode

abstract class AutoToManyMode
case class NoAutoToMany() extends AutoToManyMode
abstract class AutoToMany extends AutoToManyMode
case class AutoToManySet() extends AutoToMany
case class AutoToManyAppend() extends AutoToMany