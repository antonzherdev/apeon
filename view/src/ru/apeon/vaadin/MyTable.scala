package ru.apeon.vaadin

import com.vaadin.ui.Table

/**
 * @author Anton Zherdev
 */

class MyTable extends Table {
  def getMyContainerDataSource : MyIndexedContainer = super.getContainerDataSource.asInstanceOf[MyIndexedContainer]

  def getId : Int = getMyContainerDataSource.getId(getValue)

  def apply[T](itemId : Object, propertyId : Object) : T =
    getMyContainerDataSource.apply(itemId, propertyId)

  def apply[T](propertyId : Object) : T =
    getMyContainerDataSource.apply(getValue, propertyId)
}