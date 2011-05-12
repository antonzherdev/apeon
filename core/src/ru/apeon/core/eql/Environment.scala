package ru.apeon.core.eql

import collection.mutable.Stack
import ru.apeon.core.script.{ObjectModel}

trait Environment{
  def model : ObjectModel
  def fromOption(alias : String) : Option[From]
  def from(alias : String) : From =
    fromOption(alias).getOrElse(
      throw new RuntimeException("Alias \"%s\" not found.".format(alias))
    )
  def from : From
  def push(from : From)
  def pop()

  def withDot[A](newDot : Dot)( f : => A) : A = {
    val old = dot
    setDot(Some(newDot))
    val ret = f
    setDot(old)
    ret
  }

  def dot : Option[Dot]
  def setDot(v : Option[Dot])
}

class DefaultEnvironment(val model : ObjectModel) extends Environment {
  val froms : Stack[From] = new Stack[From]

  def fromOption(alias: String) = froms.foldRight(None : Option[From]){(from, ret) =>
    ret.orElse{
      from.fromOption(alias)
    }
  }

  def push(from: From) {
    froms.push(from)
  }

  def pop() {
    froms.pop()
  }

  def from = froms.last

  def setDot(v: Option[Dot]) {
    dot = v
  }
  var dot : Option[Dot] = None
}