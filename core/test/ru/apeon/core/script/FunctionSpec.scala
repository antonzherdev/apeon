package ru.apeon.core.script

import ru.apeon.core.entity._
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

/**
 * @author Anton Zherdev
 */

class FunctionSpec extends Spec with ShouldMatchers with EntityDefine with ScriptDefine{
  val sh = new DefaultObjectModel
  val pack = Package(sh, "ru.apeon.core.test", "1.0.0")
  val ds = new DataSource(pack, "ds")
  sh.addDataSource(ds)
  sh.addPackage(pack)
  FillRef(sh, pack, pack)
  def run(statement : Statement*) = Script(pack, statement : _*).evaluate()

  describe("Строковые функции") {
    it("format") {
      run(Dot("test %s %s", Ref("format", Some(Seq(Par("test1"), Par("test2"))) ))) should equal ("test test1 test2")
    }
  }
}