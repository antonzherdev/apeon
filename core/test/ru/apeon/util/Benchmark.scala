package ru.apeon.util

import compat.Platform

trait Benchmark {
  def run(noTimes: Int, multiplier : Int = 1) (f : => Unit) : BenchmarkResult = {
    f //Выполнить первый раз, чтобы не попало в тест. Должны загрузиться все классы
    BenchmarkResult(for (i <- List.range(1, noTimes + 1)) yield {
      val startTime = Platform.currentTime
      var i = 0; while (i < multiplier) {
        f
        i += 1
      }
      val stopTime = Platform.currentTime
      Platform.collectGarbage

      (stopTime - startTime).toInt
    })
  }

}

case class BenchmarkResult(times : Seq[Int]) {
  def print : BenchmarkResult = {
    times.foreach(time => System.out.println(time.toDouble/1000))
    System.out.println(" Amount: " + (sum.toDouble/1000).toString)
    val ret = avg
    System.out.println("Average: " + (ret.toDouble/1000).toString)
    this
  }

  def avg : Int = {
    sum/times.size
  }

  def sum : Int = {
    times.foldLeft(0){_ + _}
  }
}