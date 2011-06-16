package ru.apeon.core.script

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * @author Anton Zherdev
 */

class TupleSuite extends FunSuite with ShouldMatchers with ScriptTest  {
  test("parser") {
    ScriptParser.parse(model, CoreModule, pack, "(\"5\", 1, \"3\")._2") should equal(
      script(tuple("5", 1, "3") ~ ref("_2"))
    )
  }

  test("run") {
    run(tuple("5", 1, "3") ~ ref("_2")) should equal(1)
  }
}