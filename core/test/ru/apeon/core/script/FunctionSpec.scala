package ru.apeon.core.script

import ru.apeon.core.entity._
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

/**
 * @author Anton Zherdev
 */

class FunctionSpec extends Spec with ShouldMatchers with EntityDefine with ScriptDefine{
  val sh = new DefaultObjectModel
  val pack = Package( "ru.apeon.core.test")
  val ds = new DataSource(pack, "ds")
  sh.addDataSource(ds)
  FillRef(sh, pack, pack)
  def run(statement : Statement*) = Script(sh, pack, statement : _*).evaluate()

  describe("Строковые функции") {
    it("format") {
      run(Dot("test %s %s", Ref("format", Some(Seq(Par("test1"), Par("test2"))) ))) should equal ("test test1 test2")
    }
  }
}