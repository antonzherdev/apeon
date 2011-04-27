package ru.apeon.core.loader


case class Module(name : String, version : Seq[Int], path : String,
                         listeners : Seq[String])