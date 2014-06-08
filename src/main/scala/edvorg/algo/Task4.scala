package edvorg.algo

import scala.io.Source

object Task4 extends App {

  val file = "SCC.txt"
  val verticesCount = 875714
  val f1 = new Array[Int](verticesCount + 1)
  val f2 = new Array[Int](verticesCount + 1)

  def loadGraph(file: String, from: Int, to: Int) = {
    val graph = collection.mutable.Map[Int, Array[Int]]()
    val ends = new Array[Int](verticesCount + 1)

    {
      val counts = new Array[Int](verticesCount + 1)

      {
        val source = Source.fromFile(file).getLines
        while (source.hasNext) {
          val edge = source.next.split(' ').map{ _.toInt }
          counts(edge(from)) += 1
        }
      }

      println("counts")

      var i = 1

      do {
        graph(i) = new Array(counts(i))

        i += 1
      }
      while (i <= verticesCount)
    }

    println("arrays")

    {
      val source = Source.fromFile(file).getLines
      while (source.hasNext) {
        val edge = source.next.split(' ').map{ _.toInt }
        graph(edge(from)).update(ends(edge(from)), edge(to))
        ends(edge(from)) += 1
      }
    }

    println("loaded")

    graph
  }

  def first(graph: collection.mutable.Map[Int, Array[Int]]) {
    println("dfs1")

    val visited1 = new Array[Boolean](verticesCount + 1)
    var s = 0
    var t = 0
    var i = verticesCount

    def dfs1(i: Int) {
      visited1(i) = true

      if (graph.isDefinedAt(i)) graph(i) foreach { i =>
        if (!visited1(i)) {
          dfs1(i)
        }
      }

      t += 1
      f1(i) = t
      f2(t) = i
    }

    do {
      if (!visited1(i)) {
        s = i
        dfs1(i)
      }
      i -= 1
    }
    while (i > 0)
  }

  def second(graph: collection.mutable.Map[Int, Array[Int]]) {
    println("dfs2")

    val visited2 = new Array[Boolean](verticesCount + 1)
    var groupSize = 0
    var i = verticesCount

    object reverse extends Ordering[Int] {
      override def compare(a: Int, b: Int) = - (a compare b)
    }

    val res = collection.mutable.PriorityQueue[Int](0, 0, 0, 0, 0)(reverse)

    def dfs2(i: Int) {
      visited2(i) = true
      groupSize += 1

      if (graph.isDefinedAt(f2(i))) graph(f2(i)) foreach { i =>
        if (!visited2(f1(i))) {
          dfs2(f1(i))
        }
      }
    }

    do {
      if (!visited2(i)) {
        groupSize = 0
        dfs2(i)
        res.enqueue(groupSize)
        res.dequeue()
      }
      i -= 1
    }
    while (i > 0)

    println(res.toList.sorted.reverse.mkString(","))
  }

  first(loadGraph(file, 1, 0))
  second(loadGraph(file, 0, 1))
}
