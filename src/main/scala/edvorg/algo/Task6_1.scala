package edvorg.algo

import io.Source

object Task6_1 extends App {
  // val arr = Array(1, 2, 3, 4, 5).sorted.map { _.toLong }
  // val arr = Array(0, 1, 2, 3, -1).sorted.map { _.toLong }
  val arr = Source.fromFile("algo1_programming_prob_2sum.txt").getLines.map { _.toLong }.toVector.sorted

  val min = - 10000
  val max = 10000

  // val min = -2
  // val max = 6

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
		println(s"x ${arr(i)} y ${arr(j)} t ${arr(i) + arr(j)} count $count")
		println(s"nextj $nextj")
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
