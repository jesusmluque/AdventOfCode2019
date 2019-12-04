def checkNumber(num: Int): Boolean = {
  val str = num.toString.split("")
  val res  = str.foldLeft((0, true, false)) { (acc, d) =>
    if (!acc._2)
      acc
    else if (d.toInt > acc._1)
      (d.toInt, true, acc._3)
    else if (!acc._3 && d.toInt == acc._1)
      (acc._1, acc._2, true)
    else if (d.toInt < acc._1)
      (acc._1, false, acc._3)
    else
      acc
  }
  res._2 && res._3
}

def count(from: Int, to: Int) = {
  (from to to).count(checkNumber)
}

count(1, 100)