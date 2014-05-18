package edvorg.algo

object Util {
  def exchange(arr: Array[Int], i: Int, j: Int) {
	val tmp = arr(i)
	arr(i) = arr(j)
	arr(j) = tmp
  }
}
