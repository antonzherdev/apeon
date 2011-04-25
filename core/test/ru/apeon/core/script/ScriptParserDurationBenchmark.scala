package ru.apeon.core.script

import org.scalatest.FunSuite
import ru.apeon.util.Benchmark
import org.scalatest.matchers.ShouldMatchers

class ScriptParserDurationBenchmark extends FunSuite with Benchmark with ShouldMatchers {
  test("Test1") {
    val model = new DefaultObjectModel
    run(5) {
      ScriptParser.parse(model, """
package ru.apeon.core.synch.comtec(0.1.0) {
    default datasource comtec
}
datasource comtec

object Format {
    def str(entityName : string, id : int, name : string) : string = "%s(%d: %s)".format(entityName, id, name)
    def doc(entityName : string, id : int, date : date, number : string) : string = "%s(%d: %tF №%s)".format(entityName, id, date, number)
    def sub(entityName : string, id : int, number : int, name : string, doc : string) : string =
        "%s(%d: %s -> %d: %s)".format(entityName, id, doc, number, name)

  	def sub(entityName : string, id : int, number : int, doc : string) : string =
        "%s(%d: %s -> %d)".format(entityName, id, doc, number)
}""")
    }.print.avg should be < (50)
  }

}