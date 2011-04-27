package ru.apeon.core.loader


trait Listener {
  def load()
  def unload()
}