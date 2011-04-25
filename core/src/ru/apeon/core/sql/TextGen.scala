package ru.apeon.core.sql

import scala.{StringBuilder}

trait TextGen{
  var sb : StringBuilderEx = _

  def textGen()

  final def doTextGen(_sb : StringBuilderEx) {
    sb = _sb
    textGen()
    sb = null
  }

  override def toString = {
    toString(Map.empty)
  }

  def toString(parameters : scala.collection.Map[String, Any]) = {
    val sb : StringBuilderEx = new StringBuilderEx
    sb.parameters = parameters
    doTextGen(sb)
    sb.toString
  }

  def param(name : String) : Option[Any] = sb.parameters.get(name)

  def append(tg : TextGen) {
    tg.doTextGen(sb)
  }
  def append[T <: TextGen](tg : Option[T]) {
    tg match {
      case Some(t) => t.doTextGen(sb)
      case None => {}
    }
  }
  def append(s : String) {
    sb.sb.append(s)
  }
  def append(c : Char) {
    sb.sb.append(c)
  }
  def append(x : Any) {
    sb.sb.append(x)
  }
  def line( f : => Unit) {
    sb.line(f)
  }
  def indent( f : => Unit) = sb.indent(f)

  def append[T <: TextGen](l : Seq[T], del : String) {
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