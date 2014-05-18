package edvorg.algo

import scala.io.Source
import scala.annotation._

object Task2 {
  private var comparisons = 0

  def quicksortImperative(arr: Array[Int]) {
    def impl(begin: Int, size: Int) {
      def choosePivotMedian = {
        val median = math.max(size - 1, 0) / 2
        if (arr(begin + median) <= arr(begin) && arr(begin) <= arr(begin + size - 1)) 0
        else if (arr(begin) <= arr(begin + median) && arr(begin + median) <= arr(begin + size - 1)) median
        else size - 1
      }
      def choosePivotFirst = 0
      def choosePivotLast = size - 1

      def choosePivot = choosePivotMedian

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

        comparisons += math.max(split - 1 - 1, 0)
        comparisons += math.max(size - split - 1, 0)
      }
    }

    comparisons = 0
    impl(0, arr.size)
  }

  def main(args: Array[String]) {
	val numbers = Source.fromFile("QuickSort.txt").getLines.toArray.map { _.toInt }
    quicksortImperative(numbers)
    println(s"number of comparisons: $comparisons")
  }
}
