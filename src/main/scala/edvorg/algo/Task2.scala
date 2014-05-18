package edvorg.algo

import scala.io.Source
import scala.annotation._

object Task2 {
  private[this] val random = new scala.util.Random(System.currentTimeMillis())
  private[this] def r = random.nextInt
  private[this] def r(p: Int) = random.nextInt(p)

  def quicksortImperative(arr: Array[Int]) {
    def impl(begin: Int, size: Int) {
      def choosePivot(size: Int) = 0

      def exchange(i: Int, j: Int) {
        val tmp = arr(i)
        arr(i) = arr(j)
        arr(j) = tmp
      }

      @tailrec
      def partition(i: Int, split: Int, begin: Int, size: Int): Int =
        if (i >= size) {
          exchange(begin, begin + split - 1)
          split
        }
        else if (arr(begin + i) < arr(begin)) {
          exchange(begin + split, begin + i)
          partition(i + 1, split + 1, begin, size)
        }
        else partition(i + 1, split, begin, size)

      if (size > 1) {
        exchange(begin + choosePivot(size), begin)

        val split = partition(1, 1, begin, size)
        impl(begin, split - 1)
        impl(begin + split, size - split)
      }
    }

    impl(0, arr.size)
  }

  def main(args: Array[String]) {
	val numbers = Source.fromFile("QuickSort.txt").getLines.toArray.map { _.toInt }

    val arr = Array(4, 3, 1, 5, 7, 6, 8, 2)
    quicksortImperative(arr)
    arr foreach (x => println(x))
  }
}
