package ru.apeon.core.eql

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import ru.apeon.core.entity._
import ru.apeon.core.script._

/**
 * @author Anton Zherdev
 */

class EnvironmentSuite extends FunSuite with ShouldMatchers{
  val sh = new DefaultObjectModel
  val pack = Package(sh, "ru.apeon.core.test", "1.0.0")
  val test1 = Description(pack, "test1", Table("", "test1"), Seq(Id))

  test("Alias1") {
    val env = new DefaultEnvironment(sh)
    env.fromOption("s") should equal(None)
    val f = From(test1, "s")
    env.push(f)
    env.fromOption("s") should equal(Some(f))
    env.pop()
    env.fromOption("s") should equal(None)
  }

  test("Alias2") {
    val env = new DefaultEnvironment(sh)
    env.fromOption("a") should equal(None)
    env.fromOption("s") should equal(None)
    val fa = From(test1, "a")
    val fs = From(test1, "s")
    env.push(fa)
    env.push(fs)
    env.fromOption("s") should equal(Some(fs))
    env.fromOption("a") should equal(Some(fa))
    env.pop()
    env.fromOption("s") should equal(None)
    env.fromOption("a") should equal(Some(fa))
    env.pop()
    env.fromOption("s") should equal(None)
    env.fromOption("a") should equal(None)
  }
}