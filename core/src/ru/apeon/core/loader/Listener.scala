package ru.apeon.core.loader


trait Listener {
  def preLoad()
  def load()
  def unload()
}