package ru.apeon.sync

case class SyncOptions(sync : SyncMode = InsertUpdate(), auto : AutoMode = AutoUpdate(),
                       optimization : Set[Optimization] = Set())
{
  def hasOptimization(o : Optimization) = optimization.contains(o)
  def addOptimization(o : Optimization) : SyncOptions =
    SyncOptions(sync, auto, optimization + o)
}

abstract class SyncMode
case class InsertOnly() extends SyncMode
case class InsertUpdate() extends SyncMode
case class UpdateOnly() extends SyncMode

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

abstract class Optimization
case class HashIndexOptimization() extends Optimization