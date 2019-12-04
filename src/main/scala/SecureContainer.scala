object SecureContainer {

  def count(from: Int, to: Int) = {
    (from to to).count(checkNumber(false))
  }

  def countStrict(from: Int, to: Int) = {
    (from to to).count(checkNumber(true))
  }

  def checkNumber(strict: Boolean)(num: Int): Boolean = {
    checkConsecutiveDigits(num) && checkExistAPair(num, strict)
  }

  private def checkExistAPair(num: Int, strict: Boolean) = {
    val str = num.toString.split("")
    val res = str.foldLeft(List[String]()) { (acc, n) =>
      if (acc.nonEmpty && acc.head.head.toString == n)
        acc.head ++ n :: acc.tail
      else
        n :: acc
    }.count(r => (r.length == 2 && strict) || (!strict && r.length >= 2))
    res > 0
  }

  private def checkConsecutiveDigits(num: Int) = {
    val str = num.toString.split("")
    val res = str.foldLeft((0, true)) { (acc, d) =>
      if (!acc._2)
        acc
      else if (d.toInt > acc._1)
        (d.toInt, true)
      else if (d.toInt < acc._1)
        (acc._1, false)
      else
        acc
    }
    res._2

  }
}
