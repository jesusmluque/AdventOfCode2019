import scala.collection.immutable.Queue

val right = "R(.*)".r
val up = "U(.*)".r
val down = "D(.*)".r
val left = "L(.*)".r
def createPath(path: String):List[(Int, Int)] = {
  path.split(",").foldLeft(List((0,0))) { (acc, nextIns) =>
    nextIns match {
      case  right(x) => (1 to x.toInt).toList.foldLeft(acc)((acc2, y) => (acc.head._1, acc.head._2 + y) :: acc2)
      case  left(x) => (1 to x.toInt).toList.foldLeft(acc)((acc2, y) => (acc.head._1, acc.head._2 - y) :: acc2)
      case  down(x) => (1 to x.toInt).toList.foldLeft(acc)((acc2, y) => (acc.head._1 - y, acc.head._2) :: acc2)
      case  up(x) => (1 to x.toInt).toList.foldLeft(acc)((acc2, y) => (acc.head._1 + y, acc.head._2) :: acc2)
    }
  }
}
def findClosestCross(path1: String, path2: String) = {
  createPath(path1).toSet.intersect(createPath(path2).toSet).filter(x => x._1 != 0 && x._2 != 0)
}
findClosestCross("R8,U5,L5,D3", "U7,R6,D4,L4")

createPath("R8,U5,L5,D3").reverse
createPath("U7,R6,D4,L4").reverse


val q = Queue()
val a = q.enqueue(1).enqueue(2).enqueue(3)
a.tail