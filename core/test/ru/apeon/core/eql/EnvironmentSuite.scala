package ru.apeon.core.eql

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import ru.apeon.core.entity._
import ru.apeon.core.script.Package

/**
 * @author Anton Zherdev
 */

class EnvironmentSuite extends FunSuite with ShouldMatchers with EntityDefine {
  val test1 = desc("test1").decl(Id).b
  fillRef()

  test("Alias1") {
    val env = new DefaultEnvironment
    env.fromOption("s") should equal(None)
    val f = From(test1, "s")
    env.push(f)
    env.fromOption("s") should equal(Some(f))
    env.pop()
    env.fromOption("s") should equal(None)
  }

  test("Alias2") {
    val env = new DefaultEnvironment
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