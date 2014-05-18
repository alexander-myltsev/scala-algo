package edvorg.algo

import scala.io.Source

object Task2 {
  def main(args: Array[String]) {
	val numbers = Source.fromFile("QuickSort.txt").getLines.toArray.map { _.toInt }
  }
}
