import org.scalatest.FlatSpec

import scala.io.Source

class RocketEquationTest  extends FlatSpec  {

  lazy val massesFromFile = Source.fromResource("RocketEq.txt").getLines.toList.map(_.toLong)

  "For a mass of 12" should "get 2" in {
    assert(RocketEquation.getFuel(List(12L)) === 2)
  }

  "For a list of masses from file" should "get 3173518" in {
    assert(RocketEquation.getFuel(massesFromFile) === 3173518)
  }

  "For a list of masses with only 12" should "get 2" in {
    assert(RocketEquation.getTotalFuel(List(12L)) === 2)
  }

  "For a list of masses with only 1969" should "get 966" in {
    assert(RocketEquation.getTotalFuel(List(1969L)) === 966L)
  }

  "For a list of masses with only 100756" should "get 966" in {
    assert(RocketEquation.getTotalFuel(List(100756L)) === 50346L)
  }

  "For a list of masses with  100756 and other with 1969" should "get 102725" in {
    assert(RocketEquation.getTotalFuel(List(100756L, 1969L)) === 51312L)
  }

  "For a list of masses with second method from file" should "get 4757427" in {
    assert(RocketEquation.getTotalFuel(massesFromFile) === 4757427)
  }
}
