package edvorg.algo

import scala.io.Source

object Task1 {
  def main(args: Array[String]) {
	val numbers = Source.fromFile("IntegerArray.txt").getLines.toArray.map { _.toInt }

	def mergeAndCountInversions(left: Array[Int], right: Array[Int]) = {
	  def impl(left: Array[Int], right: Array[Int], full: Array[Int], inv: Long): (Array[Int], Long) =
		if (left.isEmpty && right.isEmpty) (full, inv)
		else if (left.isEmpty) impl(left, right.tail, full ++ Array(right.head), inv)
		else if (right.isEmpty) impl(left.tail, right, full ++ Array(left.head), inv)
		else if (left.head <= right.head) impl(left.tail, right, full ++ Array(left.head), inv)
		else impl(left, right.tail, full ++ Array(right.head), inv + left.length)

	  impl(left, right, Array(), 0)
	}

	def countInversions(list: Array[Int]): (Array[Int], Long)  = {
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
