import scala.annotation.tailrec

object RocketEquation {

  def getTotalFuel(masses: List[Long]) = {
    @tailrec
    def getFuel(acc: Long, mass: Long): Long = {
      val fuel = (mass / 3L) - 2L
      if (fuel <= 0L) acc else {
        getFuel(acc + fuel, fuel)
      }
    }

    masses.foldLeft(0L) { (acc, mass) =>
      getFuel(acc, mass)
    }
  }

  def getFuel(masses: List[Long]) = masses.foldLeft(0L) { (acc, n) =>
    acc + (n / 3) - 2
  }

}
