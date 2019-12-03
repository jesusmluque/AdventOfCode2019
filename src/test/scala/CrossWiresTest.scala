import org.scalatest.FlatSpec

import scala.io.Source

class CrossWiresTest extends FlatSpec {

  "Find the cross point closest to central port of R8,U5,L5,D3 and U7,R6,D4,L4 " should "be 6" in {
    assert(CrossWires.findClosestCross("R8,U5,L5,D3", "U7,R6,D4,L4") === 6)
  }

  "Find the cross point closest to central port of R75,D30,R83,U83,L12,D49,R71,U7,L72 and U62,R66,U55,R34,D71,R55,D58,R83 " should "be 159" in {
    assert(CrossWires.findClosestCross("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83") === 159)
  }

  "Find the cross point closest to central port of R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51 and U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 " should "be 159" in {
    assert(CrossWires.findClosestCross("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") === 135)
  }

  "Find the cross point closest to central from file " should "be 2180" in {
    val list = Source.fromResource("CrossWires.txt").getLines().toList
    assert(CrossWires.findClosestCross(list(0), list(1)) === 2180)
  }

  "The minimum steps for R75,D30,R83,U83,L12,D49,R71,U7,L72 and U62,R66,U55,R34,D71,R55,D58,R83  " should "be 610" in {
    assert(CrossWires.findMinSteps("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83") === 610)
  }

  "The minimum steps for R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51 and U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 " should "be 410" in {
    assert(CrossWires.findMinSteps("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") === 410)
  }

  "The minimum steps for R8,U5,L5,D3 and U7,R6,D4,L4 " should "be 30" in {
    assert(CrossWires.findMinSteps("R8,U5,L5,D3", "U7,R6,D4,L4") === 30)
  }

  "The minimum steps for from file " should "be 112316" in {
    val list = Source.fromResource("CrossWires.txt").getLines().toList
    assert(CrossWires.findMinSteps(list(0), list(1)) === 112316)
  }
}
