package edvorg.algo

import scala.annotation.tailrec
import scala.io.Source

object Task1 {
  def main(args: Array[String]) {
    val numbers = Source.fromFile("IntegerArray.txt").getLines().toArray.map { _.toInt }

    def mergeAndCountInversions(left: Array[Int], right: Array[Int]): (Array[Int], Long) = {
      val mergedArray = Array.fill(left.length + right.length)(0)
      @tailrec
      def inversions(leftIdx: Int, rightIdx: Int, idx: Int, inv: Long): Long = {
        val leftEmpty  = leftIdx  == left.length
        val rightEmpty = rightIdx == right.length

        if (leftEmpty && rightEmpty) inv
        else if (leftEmpty)
          inversions(leftIdx, rightIdx + 1, { mergedArray(idx) = right(rightIdx); idx + 1 }, inv)
        else if (rightEmpty || left(leftIdx) <= right(rightIdx))
          inversions(leftIdx + 1, rightIdx, { mergedArray(idx) = left(leftIdx); idx + 1 }, inv)
        else
          inversions(leftIdx, rightIdx + 1, { mergedArray(idx) = right(rightIdx); idx + 1 }, inv + left.length - leftIdx)
      }

      (mergedArray, inversions(0, 0, 0, 0))
    }

    def countInversions(list: Array[Int]): (Array[Int], Long) = {
      if (list.size <= 1) (list, 0)
      else {
        val (left, right) = list.splitAt(list.length / 2)
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
