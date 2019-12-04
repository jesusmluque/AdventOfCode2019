object SecureContainer {

  type Number = Array[String]

  def count(from: Int, to: Int) = (from to to).count(checkNumber(false))

  def countStrict(from: Int, to: Int) = (from to to).count(checkNumber(true))

  private def checkNumber(strict: Boolean)(num: Int): Boolean = {
    val numS: Array[String] = getNumber(num)
    checkConsecutiveDigits(numS) && checkExistAPair(numS, strict)
  }

  private def checkExistAPair(num: Number, strict: Boolean) = num.foldLeft(List[String]()) { (acc, n) =>
    if (acc.nonEmpty && acc.head.head.toString == n)
      acc.head ++ n :: acc.tail
    else
      n :: acc
  }.count(r => (r.length == 2 && strict) || (!strict && r.length >= 2)) > 0

  private def checkConsecutiveDigits(num: Number) = num.foldLeft((0, true)) { (acc, d) =>
    acc match {
      case (_, false) => acc
      case (n, _) if d.toInt > n => (d.toInt, true)
      case (n, _) if d.toInt < n => (acc._1, false)
      case _ => acc
    }
  }._2

  private def getNumber(n: Int) = {
    val numS = n.toString.split("")
    numS
  }
}
