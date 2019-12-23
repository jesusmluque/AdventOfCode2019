case class Matrix[A](elements: Vector[Vector[A]]) {
  def map[B](f: A => B) = Matrix(elements.map(m => m.map(f)))
  def foldLeft[B](z: B)(f: (B, A) => B) = elements.foldLeft(z) { (acc, v) =>
    v.foldLeft(acc)(f)
  }
  def zipWithIndex = Matrix(elements.zipWithIndex.map(r => r._1.zipWithIndex.map(c => (c._1, (r._2, c._2)))))
  def getValue(x: Int, y: Int): A = elements(x)(y)
  def update(element: A, position: (Int, Int)) = Matrix(elements.updated(position._1, elements(position._1).updated(position._2, element)))
  def filter(p: A => Boolean) = elements.flatMap(m => m.filter(p))
}

case class MonitoringStation(elements: Matrix[String]) {


  type Node = (String, (Int, Int))

  def findAsteroid(index: Int):Option[(Int, Int)] = {
    val startPosition = findBest()._2

    def updateByQuadrant(map: Matrix[String], intermedian: Matrix[((String, (Int, Int)), (Double, Int))], quadrant: Int, count: Int, targetPos: Int) = {
      intermedian.filter(p => p._2._2 == quadrant && p._1._1 == "#").sortWith { (a, b) =>
        if (a._2._1 * -1 > b._2._1 * -1)
          true
        else if (a._2._1 == b._2._1 && closeToTarget(startPosition, a._1._2, b._1._2))
          true
        else
          false
      }.foldLeft(List[((String, (Int, Int)), (Double, Int))]()) { (acc, n) =>
        if (acc.nonEmpty && acc.head._2._1 == n._2._1)
          acc
        else
          n :: acc
      }.reverse.foldLeft((map, count, (-1,-1))) { (acc, n) =>
        (acc._1.update(".", n._1._2), acc._2 + 1, if (acc._2 + 1 == targetPos) n._1._2 else acc._3)
      }
    }

    def find(map: Matrix[String], targetPos: Int, currentCount: Int): (Int, Int) = {
      val intermedian = map.zipWithIndex.map { c =>
        if (c._2 == startPosition)
          (("current", c._2), tilt(c._2, startPosition))
        else
          (c, tilt(c._2, startPosition))
      }
      val map1 = updateByQuadrant(map, intermedian, 1, currentCount, targetPos)
      if (map1._3 != (-1,-1))
        map1._3
      else {
        val map2 = updateByQuadrant(map1._1, intermedian, 2, map1._2, targetPos)
        if (map2._3 != (-1,-1))
          map2._3
        else {
          val map3 = updateByQuadrant(map2._1, intermedian, 3, map2._2, targetPos)
          if (map3._3 != (-1,-1))
            map3._3
          else {
            val map4 = updateByQuadrant(map3._1, intermedian, 4, map3._2, targetPos)
            if (map4._3 != (-1,-1))
              map4._3
            else if (map4._1.filter(_ == "#").size > 1)
              find(map4._1, targetPos, map4._2)
            else
              map4._3
          }
        }

      }
    }
    val res = find(this.elements, index, 0)
    if (res != (-1,-1))
      Some(res)
    else
      None
  }

  def findBest() = max(elements.zipWithIndex.map(c => detectAll(c)))

  private def max(m: Matrix[Int]) =m.zipWithIndex.foldLeft((0, (-1,-1))) { (acc, n) =>
    if (acc._1 < n._1)
      n
    else
      acc
  }

  private def detectAll(current: Node) = {
    if (current._1 == ".")
      0
    else elements.zipWithIndex.map { n =>

      if (n == current)
        ("current", tilt(n._2, current._2))
      else
        (n._1, tilt(n._2, current._2))

    }.foldLeft(Set[(String, (Double, Int))]()) { (acc, n) =>
      if (n._1 == "#")
        acc + n
      else
        acc
    }.count(_._1 != "current")
  }

  private def tilt(p1: (Int, Int), p2: (Int, Int)) = {
    val deltaX = p2._1 - p1._1
    val deltaY = p2._2 - p1._2
    val quadrant = if (deltaX <= 0 && deltaY <= 0) 2 else if (deltaX <= 0 && deltaY > 0) 3 else if (deltaX > 0 && deltaY > 0 ) 4 else 1

    if (p1 == p2)
      (1000000D, quadrant)
    else if (deltaY == 0)
      (1000000D * Math.pow(-1, quadrant), quadrant)
    else
      (deltaX.toDouble / deltaY.toDouble, quadrant)
  }

  private def closeToTarget(target: (Int, Int), a: (Int, Int), b: (Int, Int)) = {
    val absA = (a._1 - target._1)*(a._1 - target._1) + (a._2 - target._2)*(a._2 - target._2)
    val absB = (b._1 - target._1)*(b._1 - target._1) + (b._2 - target._2)*(b._2 - target._2)

    absA < absB
  }
}
object MonitoringStation {

  def apply(elements: String): MonitoringStation = new MonitoringStation(
    Matrix(elements.split("\n").toVector.map(_.split("").toVector))
  )

}
