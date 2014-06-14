package edvorg.algo

import io.Source

object Task6_1 extends App {
  val arr = Source.fromFile("algo1_programming_prob_2sum.txt").getLines.map { _.toLong }
	.toVector.sorted

  val min = - 10000
  val max = 10000

  var count = 0
  var i = 0
  var nextj = arr.length - 1
  val visited = new Array[Boolean](max - min + 1)

  while (i < arr.length) {

	val x = arr(i)
	val nextx = if (i +1 < arr.length) arr(i + 1) else x

	var j = nextj
	var y = arr(j)
	var t = x + y
	var nextt = nextx + y

	while (i < j && min <= t) {
	  if (nextt > max) {
		nextj = j
	  }

	  if (t <= max && !visited(t.toInt - min)) {
		visited(t.toInt - min) = true
		count += 1
	  }

	  j -= 1
	  y = arr(j)
	  t = x + y
	  nextt = nextx + y
	}

	i += 1
  }

  println(s"count: $count")
}
