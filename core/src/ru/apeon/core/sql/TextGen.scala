package ru.apeon.core.sql

import scala.{StringBuilder}

trait TextGen{
  var sb : StringBuilderEx = _

  def toString(o : Object, parameters : scala.collection.Map[String, Any]) : String = {
    sb = new StringBuilderEx
    sb.parameters = parameters
    append(o)
    val ret = sb.toString
    sb = null
    ret
  }

  protected def appendFunction : PartialFunction[Any, Unit]

  protected final def append(o : Any) {
    appendFunction.lift(o).getOrElse(sb.sb.append(o))
  }

  protected final def append(o : Option[Any]) {
    if(o.isDefined) {
      append(o.get)
    }
  }

  def param(name : String) : Option[Any] = sb.parameters.get(name)

  def line( f : => Unit) {
    sb.line(f)
  }
  def indent( f : => Unit) = sb.indent(f)

  def append(l : Seq[Any], del : String) {
    l.foldLeft(false) {
      (b, i) =>
        if (b) append(del)
        append(i)
        true
    }
  }

  def option[T](pref : Any, o : Option[T]) {
    if (o.isDefined) {
      append(pref)
      append(o.get)
    }
  }

  def option[T](pref : Any, o : Option[T], suf : Any) {
    if (o.isDefined) {
      append(pref)
      append(o.get)
      append(suf)
    }
  }
}

class StringBuilderEx  {
  var parameters : scala.collection.Map[String, Any] = Map.empty
  var sb : StringBuilder = new StringBuilder
  var indents : Int = 0

  def indent( f : => Unit ) = {
    indents += 1
    f
    indents -= 1
  }

  def line( f : => Unit) {
    if(sb.length != 0) sb.append('\n')

    var i : Int = 0
    while(i < indents) {
      sb.append('\t')
      i += 1
    }
    f
  }


  override def toString = sb.toString()
}