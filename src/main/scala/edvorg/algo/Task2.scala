package edvorg.algo

import scala.io.Source
import scala.annotation._

object Task2 {
  private var comparisons = 0

  def quicksortImperative(arr: Array[Int], choosePivot: (Array[Int], Int, Int) => Int) {
    def impl(begin: Int, size: Int) {
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
        val pivot = choosePivot(arr, begin, size)
        if (pivot != 0) Util.exchange(arr, begin + pivot, begin)

        val split = partition(1, 1)

        comparisons += math.max(split - 1, 0)
        comparisons += math.max(size - split, 0)

        impl(begin, split - 1)
        impl(begin + split, size - split)
      }
    }

    comparisons = 0
    impl(0, arr.size)
  }

  def choosePivotMedian(arr: Array[Int], begin: Int, size: Int) = {
    val median = if (size % 2 == 0) size / 2 - 1 else size / 2
    if (arr(begin + median) < arr(begin) &&
          arr(begin) < arr(begin + size - 1)) 0
    else if (arr(begin + size - 1) < arr(begin) &&
               arr(begin) < arr(begin + median)) 0

    else if (arr(begin) < arr(begin + median) &&
               arr(begin + median) < arr(begin + size - 1)) median
    else if (arr(begin + size - 1) < arr(begin + median) &&
               arr(begin + median) < arr(begin)) median

    else size - 1
  }

  def choosePivotFirst(arr: Array[Int], begin: Int, size: Int) = 0
  def choosePivotLast(arr: Array[Int], begin: Int, size: Int) = size - 1
  def choosePivotRandom(arr: Array[Int], begin: Int, size: Int) = Rand.r(size)

  def main(args: Array[String]) {
	val numbers = Source.fromFile("QuickSort.txt").getLines.toArray.map { _.toInt }
    quicksortImperative(numbers, choosePivotFirst)
    println(s"number of comparisons: $comparisons")
  }
}
