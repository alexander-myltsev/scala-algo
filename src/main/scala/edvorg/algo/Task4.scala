package edvorg.algo

import scala.io.Source

object Task4 extends App {
  var vertexCount = 0
  val graph = Source.fromFile("SCC.txt").getLines.map{ _.split(' ').map{ _.toInt } }
    .toArray.groupBy{ _.head }.map{ case (k, v) => (k, v map { _.tail.head })}

  println("Task4")
}
