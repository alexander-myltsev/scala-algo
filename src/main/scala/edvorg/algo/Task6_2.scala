package edvorg.algo

import io.Source

object Task6_2 extends App {
  val arr = Source.fromFile("Median.txt").getLines.map { _.toLong }.toVector

  def median(k: Int) = (arr take k).sorted.apply((k + 1) / 2 - 1)

  val result = {
    for { i <- 1 to arr.length } yield median(i)
  }.sum % arr.length

  println(s"arr size: ${arr.length} result: $result")
}
