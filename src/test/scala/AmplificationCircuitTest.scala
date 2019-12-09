import org.scalatest.FlatSpec

import scala.io.Source

class AmplificationCircuitTest extends FlatSpec {

  "Max thruster for phase setting sequence 4,3,2,1,0 with program 3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" should "be 43210 " in {
    assert(AmplificationCircuit.executeStream(Vector(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0))(List(4,3,2,1,0)) === 43210)
  }

  "Max thruster for phase setting sequence 0,1,2,3,4 with program 3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0" should "be 54321 " in {
    assert(AmplificationCircuit.executeStream(Vector(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0))(List(0,1,2,3,4)) === 54321)
  }

  "Max thruster for phase setting sequence 1,0,4,3,2 with program 3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0" should "be 65210 " in {
    assert(AmplificationCircuit.executeStream(Vector(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0))(List(1,0,4,3,2)) === 65210)
  }

  "Max thruster for phase setting sequence 0,1,2,3,4 with program 3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0" should "be 65210 " in {
    assert(AmplificationCircuit.findMax(Vector(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)) === 65210)
  }

  "Max thruster with program 3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" should "be 43210 " in {
    assert(AmplificationCircuit.findMax(Vector(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)) === 43210)
  }

  "Max thruster with program 3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0" should "be 54321 " in {
    assert(AmplificationCircuit.findMax(Vector(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0)) === 54321)
  }

  "Max thruster with program from file " should "be 3" in {
    assert(AmplificationCircuit.findMax(Source.fromResource("AmplificationCircuit.txt").getLines().toVector.flatMap(_.split(",")).map(_.toInt)) === 880726)
  }

  "Max thruster signal for phase setting sequence 9,8,7,6,5 with program 3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5 " should "be 139629729" in {
    assert(AmplificationCircuit.executeFeedbackLoop(Vector(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5))(List(9,8,7,6,5)) === 139629729)
  }

  "Max thruster signal for phase setting sequence 9,7,8,5,6 with program 3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10 " should "be 18216" in {
    assert(AmplificationCircuit.executeFeedbackLoop(Vector(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10))(List(9,7,8,5,6)) === 18216)
  }

  "Max thruster with program using loop feedback from file  " should "be 4931744" in {
    assert(AmplificationCircuit.findMaxWithLoop(Source.fromResource("AmplificationCircuit.txt").getLines().toVector.flatMap(_.split(",")).map(_.toInt)) === 4931744)
  }
}
