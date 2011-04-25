package ru.apeon.core.entity

import ru.apeon.core.script._

/**
 * @author Anton Zherdev
 */

object EntityConfiguration {
  var model : ObjectModel = null

  var dataSource : DataSource = null
  var store : PersistentStore = null
  var coordinator : PersistentStoreCoordinator = null

  var manager : EntityManager = null
}

trait EntityConfiguration {
  def apply()
}

object DefaultEntityConfiguration extends EntityConfiguration {
  def apply() {
    EntityConfiguration.model = new DefaultObjectModel
    EntityConfiguration.store = new SqlPersistentStore("default")
    EntityConfiguration.coordinator = new PersistentStoreCoordinator(Seq(EntityConfiguration.store))
    EntityConfiguration.manager = new DefaultEntityManager
  }
}