package ru.apeon.core.loader

import ru.apeon.core.script.ObjectModel


trait Listener {
  def preLoad(model : ObjectModel)
  def load(model : ObjectModel)
  def unload(model : ObjectModel)
}