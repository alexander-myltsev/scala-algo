package edvorg.algo

import scala.annotation.tailrec
import scala.io.Source

/*
  1. Elapsed time: 44851932186ns

 */
object Task1 {
  def main(args: Array[String]) {
    val numbers = Source.fromFile("IntegerArray.txt").getLines().toArray.map { _.toInt }

    def mergeAndCountInversions(left: Array[Int], right: Array[Int]): (Array[Int], Long) = {
      val mergedArray = Array.fill(left.length + right.length)(0)
      @tailrec
      def inversions(leftIdx: Int, rightIdx: Int, idx: Int, inv: Long): Long =
        if (leftIdx == left.length && rightIdx == right.length) inv
        else if (leftIdx == left.length)
          inversions(leftIdx, rightIdx + 1, { mergedArray(idx) = right(rightIdx); idx + 1 }, inv)
        else if (rightIdx == right.length)
          inversions(leftIdx + 1, rightIdx, { mergedArray(idx) = left(leftIdx); idx + 1 }, inv)
        else if (left(leftIdx) <= right(rightIdx))
          inversions(leftIdx + 1, rightIdx, { mergedArray(idx) = left(leftIdx); idx + 1 }, inv)
        else
          inversions(leftIdx, rightIdx + 1, { mergedArray(idx) = right(rightIdx); idx + 1 }, inv + left.length - leftIdx)

      (mergedArray, inversions(0, 0, 0, 0))
    }

    def countInversions(list: Array[Int]): (Array[Int], Long) = {
      if (list.isEmpty || list.tail.isEmpty) (list, 0)
      else {
        val (left, right) = list splitAt list.length / 2
        val (leftSorted, leftInversions) = countInversions(left)
        val (rightSorted, rightInversions) = countInversions(right)
        val (merged, splitInversions) = mergeAndCountInversions(leftSorted, rightSorted)
        (merged, leftInversions + rightInversions + splitInversions)
      }
    }

    val (sorted, inversions) = countInversions(numbers)

    println(s"number of inversions is $inversions")
  }
}
