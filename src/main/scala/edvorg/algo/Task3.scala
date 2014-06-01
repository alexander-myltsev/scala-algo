package edvorg.algo

import scala.io.Source

object Task3 extends App {
  var vertexCount = 0
  val graph = Source.fromFile("kargerMinCut.txt").getLines.toArray flatMap {
    x => {
      vertexCount += 1
      val ints = x.split("\t") map { _.toInt }
      ints drop 1 map { x => (ints(0), x) }
    }
  }

  def minCut() = {
    Rand.reinitRand()

    val unct = (0 to graph.length - 1).toArray
    var unctend = graph.length
    var currentGroup = 1
    val groups = new Array[Int](vertexCount)
    val groupsCounts = new Array[Int](vertexCount + 1)
    var groupsEnd = 0

    def contractEdge(): Boolean = {
      var cont = true
      val edge = Rand.r(unctend)
      val (from, to) = graph(unct(edge)) match { case (f, t) => (f - 1, t - 1) }

      if (groups(from) == 0 && groups(to) == 0) {
        if (groups.length > 2) {
          groups(from) = currentGroup
          groups(to) = currentGroup
          groupsCounts(currentGroup) += 2
          currentGroup += 1
        }
        else {
          groups(from) = 1
          groups(to) = 2
          currentGroup = 2
          groupsCounts(1) += 1
          groupsCounts(2) += 1
          cont = false
        }
      }
      else if (groups(from) == 0) {
        if (groupsCounts(groups(to)) < groups.length - 1) {
          groups(from) = groups(to)
          groupsCounts(groups(to)) += 1
        }
        else {
          groups(from) = currentGroup
          groupsCounts(currentGroup) += 1
          currentGroup += 1
          cont = false
        }
      }
      else if (groups(to) == 0) {
        if (groupsCounts(groups(from)) < groups.length - 1) {
          groups(to) = groups(from)
          groupsCounts(groups(from)) += 1
        }
        else {
          groups(to) = currentGroup
          groupsCounts(currentGroup) += 1
          currentGroup += 1
          cont = false
        }
      }
      else if (groups(from) != groups(to)) {
        if (groupsCounts(groups(from)) +
              groupsCounts(groups(to)) >= groups.length) {
          cont = false
        }
        else if (groups(from) < groups(to)) {
          groupsCounts(groups(from)) += groupsCounts(groups(to))
          groupsCounts(groups(to)) = 0

          var i = 0
          val tog = groups(to)

          do {
            if (groups(i) == tog)
              groups(i) = groups(from)

            i += 1
          } while (i < groups.length)
        }
        else {
          groupsCounts(groups(to)) += groupsCounts(groups(from))
          groupsCounts(groups(from)) = 0

          var i = 0
          val fromg = groups(from)

          do {
            if (groups(i) == fromg)
              groups(i) = groups(to)

            i += 1
          } while (i < groups.length)
        }
      }

      if (cont) {
        unctend -= 1
        unct(edge) = unct(unctend)
        unct(unctend) = 0
      }

      cont
    }

    while (unctend > 0 && contractEdge()) {
    }

    var i = 0
    var mincuts = 0

    do {
      if (groups(graph(unct(i))._1 - 1) != groups(graph(unct(i))._2 - 1))
        mincuts += 1
      i += 1
    }
    while (i < unctend)

    mincuts
  }

  var currentMinCut = graph.length

  do {
    val newMinCut = minCut()
    if (newMinCut < currentMinCut) {
      println(s"cuts: ${newMinCut / 2}")
      currentMinCut = newMinCut
      if (newMinCut <= 3) currentMinCut = 0
    }
  }
  while (true && currentMinCut > 0)
}
