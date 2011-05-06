package ru.apeon.core.script

case class Module(name : String, version : Seq[Int], path : String,
                         listeners : Seq[String])

object CoreModule extends Module("ru.apeon.core", Seq(0, 2), "", Seq())