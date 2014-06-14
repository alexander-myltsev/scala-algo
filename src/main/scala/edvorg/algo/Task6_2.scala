package edvorg.algo

import io.Source

object Task6_2 extends App {
  val arr = Source.fromFile("Median.txt").getLines.map { _.toLong }.toVector.sorted

  def median(k: Int) = 1

  val result = {
    for {
      i <- 0 until arr.length
    } yield median(i + 1)
  }.sum % arr.length

  println(s"result: $result")
}
