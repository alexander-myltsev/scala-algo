package edvorg.algo

import scala.io.Source

object Task5 extends App {
  val graph = Source.fromFile("dijkstraData.txt").getLines.toStream map {
    x => {
      val tailAndHeads = x split "\t ".toArray
      val heads = tailAndHeads.drop(1).map { _ split "," map { _.toInt } toList }

      (tailAndHeads(0).toInt, heads.toList)
    }
  } toArray
}
