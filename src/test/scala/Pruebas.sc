def max(m: Vector[Vector[Int]]): (Int, Int) = m.zipWithIndex.foldLeft((0, (-1,-1))) { (acc, n) =>
  n._1.zipWithIndex.foldLeft(acc) { (acc2, l) =>
    if (acc2._1 < l._1)
      (l._1, (n._2, l._2))
    else
      acc2
  }
}._2

max(Vector(Vector(1,2,3), Vector(4,5,6), Vector(7,8,9)))

max(Vector(Vector(7,8,9), Vector(1,2,3), Vector(4,5,6)))


