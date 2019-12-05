object SecureContainer {

  type Digits = Array[String]

  def count(from: Int, to: Int): Int = (from to to).count(checkNumber(strict = false))

  def countStrict(from: Int, to: Int): Int = (from to to).count(checkNumber(strict = true))

  private def checkNumber(strict: Boolean)(num: Int): Boolean = {
    val numS: Digits = getDigits(num)
    checkConsecutiveDigits(numS) && checkExistAPair(numS, strict)
  }

  private def checkExistAPair(num: Digits, strict: Boolean) = num.foldLeft(List[String]()) { (acc, n) =>
    if (acc.nonEmpty && acc.head.head.toString == n)
      acc.head ++ n :: acc.tail
    else
      n :: acc
  }.count(r => (r.length == 2 && strict) || (!strict && r.length >= 2)) > 0

  private def checkConsecutiveDigits(num: Digits) = num.foldLeft((0, true)) { (acc, d) =>
    acc match {
      case (_, false) => acc
      case (n, _) if d.toInt > n => (d.toInt, true)
      case (n, _) if d.toInt < n => (acc._1, false)
      case _ => acc
    }
  }._2

  private def getDigits(n: Int): Digits = n.toString.split("")
}
