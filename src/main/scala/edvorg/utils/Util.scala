package edvorg.utils

object Util {
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    println("Elapsed time: " + (System.nanoTime - t0) + "ns")
    result
  }
}
