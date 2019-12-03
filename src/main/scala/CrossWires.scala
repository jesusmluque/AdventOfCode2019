object CrossWires {

  def findMinSteps(path1S: String, path2S: String) = {
    val path1 = createPath(path1S)
    val path2 = createPath(path2S)

    val crossPoints = path1.intersect(path2)

    crossPoints.map(p => countSteps(p, path1) + countSteps(p, path2)).filter(_ != 0).min

  }

  def countSteps(point: (Int, Int), path:List[(Int, Int)]) =
    path.reverse.indexOf(point)

  def findClosestCross(path1: String, path2: String):Int = findCrossPoints(path1, path2).map(x => x._1.abs + x._2.abs ).min

  def findCrossPoints(path1: String, path2: String) =
    createPath(path1).intersect(createPath(path2)).filter(x => x._1 != 0 && x._2 != 0)

  def createPath(path: String):List[(Int, Int)] = {
    val right = "R(.*)".r
    val up = "U(.*)".r
    val down = "D(.*)".r
    val left = "L(.*)".r

    path.split(",").foldLeft(List((0,0))) { (acc, nextIns) =>
      nextIns match {
        case  right(x) => (1 to x.toInt).foldLeft(acc)((acc2, y) => (acc.head._1, acc.head._2 + y) :: acc2)
        case  left(x) => (1 to x.toInt).foldLeft(acc)((acc2, y) => (acc.head._1, acc.head._2 - y) :: acc2)
        case  down(x) => (1 to x.toInt).foldLeft(acc)((acc2, y) => (acc.head._1 - y, acc.head._2) :: acc2)
        case  up(x) => (1 to x.toInt).foldLeft(acc)((acc2, y) => (acc.head._1 + y, acc.head._2) :: acc2)
      }
    }
  }

}
