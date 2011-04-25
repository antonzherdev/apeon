package ru.apeon.vaadin

import java.io.Serializable

/**
 * @author Anton Zherdev
 */

class ListenerManager[E] extends Serializable {
    type L = (E) => Unit
    private var listeners: List[L] = Nil

    /** True if the listener is already in our list. */
    private def isListener(listener:L) = listeners.exists(_==listener)

    /** Add a listener to our list if it is not already there. */
    def addListener(listener:L) = synchronized {
        if (!isListener(listener))
            listeners = listener :: listeners
    }

    /** Remove a listener from our list.  If not in the list, ignored. */
    def removeListener(listener:L):Unit = synchronized {
        listeners = listeners.filter(_!=listener)
    }

    /** Apply the given closure to all of the listeners in the list. */
    def fireEvent(event:E) = synchronized {
        listeners.foreach(_.apply(event))
    }
}