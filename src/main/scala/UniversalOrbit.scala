import scala.annotation.tailrec

object UniversalOrbit {

  def count(orbits: List[String]): Option[Int] = {
    def countOrbits(init: String, numOrbs: Int, orbits: List[String]): Int = {

      val next = init.substring(init.indexOf(")") + 1)
      val newNumOrbits = numOrbs + 1
      orbits.filter(p => p.startsWith(next)).map { n =>
        countOrbits(n, newNumOrbits, orbits.filter(_ != init))
      } match {
        case List() => newNumOrbits
        case l => l.sum + newNumOrbits
      }
    }
    orbits.find(p => p.contains("COM")).map { orbit =>
      countOrbits(orbit, 0, orbits)
    }
  }

  def countOrbitTransfer(orbits: List[String]): Option[Int] = {
    def countOrbits(init: String, numOrbs: List[String], orbits: List[String], target: String): List[List[String]] = {

      val next = init.replaceAll(target, "").filter(_ != ')')
      val branches = orbits.filter(p => p.contains(next) && p != init)
      if (next == "SAN")
        List(numOrbs)
      else
        branches.flatMap(countOrbits(_, next :: numOrbs, orbits.filter(_ != init), next))
    }
    orbits.find(p => p.contains("YOU"))
          .map(countOrbits(_, List(), orbits, "YOU")
            .filter(_.nonEmpty).minBy(_.length).length - 1)
  }
}
