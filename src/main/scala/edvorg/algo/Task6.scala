package edvorg.algo

import io.Source

object Task6_1 extends App {
  val arr = Source.fromFile("algo1_programming_prob_2sum.txt").getLines.map{_.toLong}.toVector
}
