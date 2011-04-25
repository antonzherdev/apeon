package ru.apeon.vaadin

import com.vaadin.data.util.IndexedContainer

/**
 * @author Anton Zherdev
 */

class MyIndexedContainer extends IndexedContainer with Iterable[MyIndexedContainerRow]{
  def update(itemId : Object, propertyId : Object, value : Any) =
    getContainerProperty(itemId, propertyId).setValue(value)

  def apply[T](itemId : Object, propertyId : Object) : T =
    getContainerProperty(itemId, propertyId).getValue.asInstanceOf[T]

  def getId(row : Object) : Int =
    apply[java.lang.Integer](row, "id").intValue


  override def size = {
    getItemIds.size
  }

  def iterator = new Iterator[MyIndexedContainerRow]{
    val i : java.util.Iterator[Object] = getItemIds.iterator.asInstanceOf[java.util.Iterator[Object]]
    def next() = new MyIndexedContainerRow(MyIndexedContainer.this, i.next)

    def hasNext = i.hasNext
  }
}

class MyIndexedContainerRow(val data : MyIndexedContainer, val row : Object) {
  def apply[T](propertyId : Object) : T =
    data.apply[T](row, propertyId)

  def update[T](propertyId : Object, value : Any) : Unit =
    data.update(row, propertyId, value)
}
