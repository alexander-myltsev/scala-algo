package edvorg.algo

object Rand {
  var random: scala.util.Random = null
  def reinitRand() {
	random = new scala.util.Random(System.currentTimeMillis())
  }
  reinitRand()
  def r = random.nextInt
  def r(p: Int) = random.nextInt(p)
}
