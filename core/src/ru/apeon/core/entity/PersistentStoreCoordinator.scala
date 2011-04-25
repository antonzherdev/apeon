package ru.apeon.core.entity

import ru.apeon.core.script.ObjectModel

/**
 * @author Anton Zherdev
 */

class PersistentStoreCoordinator(val stores : Seq[PersistentStore], val model : ObjectModel = EntityConfiguration.model) {
  
}

