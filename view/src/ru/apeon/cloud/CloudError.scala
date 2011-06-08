package ru.apeon.cloud

/**
 * @author Anton Zherdev
 */

class CloudError(s : String, cause: Throwable) extends Exception(s, cause) {
  def this(ss : String) = this(ss, null)
}