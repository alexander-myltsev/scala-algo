package edvorg.algo

import scala.io.Source
import collection.mutable.PriorityQueue

object Task5 extends App {
  private object orderBySecondRev extends Ordering[Vector[Int]] {
    override def compare(a: Vector[Int], b: Vector[Int]) = - (a(1) compare b(1))
  }

  val graph = Source.fromFile("dijkstraData.txt").getLines.toStream map {
    x => {
      val tailAndHeads = x split "\t ".toArray
      val heads = tailAndHeads.drop(1).map { _ split "," map { _.toInt } toVector }
      (tailAndHeads(0).toInt, PriorityQueue(heads.toVector: _*)(orderBySecondRev))
    }
  } toMap

  val maxVertex = graph.keys.max

  val isVisited = new Array[Boolean](maxVertex + 1)
  val scores = new Array[Int](isVisited.length)

  def dijkstra(visitedCount: Int, visited: List[Int]) {
    type Opt = Option[(Int, Vector[Int])]

    def grade(vertex: Int): Opt =
      if (graph(vertex).isEmpty) None
      else if (!isVisited(graph(vertex).max(0))) Some(vertex -> graph(vertex).max)
      else {
        graph(vertex).dequeue
        grade(vertex)
      }


    def combine(at: Opt, bt: Opt) = (at, bt) match {
      case (Some(a), Some(b)) => {
        if (scores(a._1) + a._2(1) < scores(b._1) + b._2(1)) at else bt
      }
      case (Some(a), None) => at
      case (None, Some(b)) => bt
      case _ => None
    }

    if (visitedCount < maxVertex && !visited.isEmpty) {
      isVisited(visited.head) = true

      val next = visited.foldLeft[Opt](None) { (acc, elem) => combine(acc, grade(elem)) }

      for ((from, to) <- next) {
        graph(from).dequeue
        scores(to(0)) = scores(from) + to(1)
        dijkstra(visitedCount + 1, to(0) :: visited)
      }
    }
  }

  dijkstra(1, List(1))

  val answer = Vector(7,37,59,82,99,115,133,165,188,197) map { x =>
    if (scores(x) == 0) 1000000 else scores(x)
  } mkString ","

  println(s"answer: $answer")
}
