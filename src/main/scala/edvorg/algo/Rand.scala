package edvorg.algo

object Rand {
  val random = new scala.util.Random(System.currentTimeMillis())
  def r = random.nextInt
  def r(p: Int) = random.nextInt(p)
}
