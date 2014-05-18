package edvorg.algo

import scala.io.Source
import scala.annotation._

object Task2 {
  def quicksortImperative(arr: Array[Int]) {
    def impl(begin: Int, size: Int) {
      def choosePivot = 0

      @tailrec
      def partition(i: Int, split: Int): Int =
        if (i >= size) {
          Util.exchange(arr, begin, begin + split - 1)
          split
        }
        else if (arr(begin + i) < arr(begin)) {
          Util.exchange(arr, begin + split, begin + i)
          partition(i + 1, split + 1)
        }
        else partition(i + 1, split)

      if (size > 1) {
        Util.exchange(arr, begin + choosePivot, begin)

        val split = partition(1, 1)
        impl(begin, split - 1)
        impl(begin + split, size - split)
      }
    }

    impl(0, arr.size)
  }

  def main(args: Array[String]) {
	val numbers = Source.fromFile("QuickSort.txt").getLines.toArray.map { _.toInt }
    quicksortImperative(numbers)
  }
}
