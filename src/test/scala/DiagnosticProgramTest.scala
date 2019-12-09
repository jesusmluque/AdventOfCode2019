import org.scalatest.FlatSpec

import scala.io.Source

class DiagnosticProgramTest extends FlatSpec {

  "The program 3,0,4,0,99 with input 1 " should "return 1 " in {
    assert(DiagnosticProgram.execute(Vector(3,0,4,0,99), List(1)).output === List(1))
  }

  "The program 3,3,11102,2,3,4,10,4,10,99,5 with input 1 " should "return 1 " in {
    assert(DiagnosticProgram.execute(Vector(3,3,11102,2,3,4,10,4,10,99,5), List(1)).output === List(12))
  }

  "The program 3,3,11102,2,3,4,10,4,10,1,5,6,16,4,16,99,8 with input 1 " should "return 1 " in {
    assert(DiagnosticProgram.execute(Vector(3,3,11102,2,3,4,10,4,10,1101,5,6,16,4,16,99,8), List(1)).output === List(18,12))
  }

  "A program 1,9,10,3,2,3,11,0,99,30,40,50 " should "becomes 3500,9,10,70,2,3,11,0,99,30,40,50" in {
    assert(DiagnosticProgram.execute(Vector(1,9,10,3,2,3,11,0,4,0,99,30,40,50), List(1)).output === List(2970))
  }

  "The program from file with input 1" should "be 11933517" in {
    val program = Source.fromResource("DiagnosticProgram.txt").getLines.flatMap(_.toString.split(",").map(_.toInt)).toVector
    assert(DiagnosticProgram.execute(program, List(1)).output === List(11933517, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  }

  "A program 3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 with input 0" should "be 0" in {
    assert(DiagnosticProgram.execute(Vector(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), List(0)).output  === List(0))
  }

  "A program 3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 with input 3" should "be 1" in {
    assert(DiagnosticProgram.execute(Vector(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), List(3)).output === List(1))
  }

  "A program 3,3,1105,-1,9,1101,0,0,12,4,12,99,1 with input 3" should "be 1" in {
    assert(DiagnosticProgram.execute(Vector(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), List(3)).output === List(1))
  }

  "A program 3,9,8,9,10,9,4,9,99,-1,8 with input 8" should "be 1" in {
    assert(DiagnosticProgram.execute(Vector(3,9,8,9,10,9,4,9,99,-1,8), List(8)).output === List(1))
  }

  "A program 3,9,8,9,10,9,4,9,99,-1,8 with input 8" should "be 0" in {
    assert(DiagnosticProgram.execute(Vector(3,9,8,9,10,9,4,9,99,-1,8), List(5)).output === List(0))
  }

  "A program 3,9,7,9,10,9,4,9,99,-1,8 with input 5" should "be 1" in {
    assert(DiagnosticProgram.execute(Vector(3,9,7,9,10,9,4,9,99,-1,8), List(5)).output === List(1))
  }

  "A program 3,9,7,9,10,9,4,9,99,-1,8 with input 9" should "be 0" in {
    assert(DiagnosticProgram.execute(Vector(3,9,7,9,10,9,4,9,99,-1,8), List(9)).output === List(0))
  }

  "A program 3,3,1108,-1,8,3,4,3,99 with input 8" should "be 1" in {
    assert(DiagnosticProgram.execute(Vector(3,3,1108,-1,8,3,4,3,99), List(8)).output === List(1))
  }

  "A program 3,3,1108,-1,8,3,4,3,99 with input 4" should "be 0" in {
    assert(DiagnosticProgram.execute(Vector(3,3,1108,-1,8,3,4,3,99), List(4)).output === List(0))
  }

  "A program 3,3,1107,-1,8,3,4,3,99 with input 5" should "be 1" in {
    assert(DiagnosticProgram.execute(Vector(3,3,1107,-1,8,3,4,3,99), List(5)).output === List(1))
  }

  "A program 3,3,1107,-1,8,3,4,3,99 with input 9" should "be 0" in {
    assert(DiagnosticProgram.execute(Vector(3,3,1107,-1,8,3,4,3,99), List(9)).output === List(0))
  }
  "A program 3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99 with input 9" should "be 1001" in {
    assert(DiagnosticProgram.execute(Vector(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99), List(9)).output === List(1001))
  }

  "A program 3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99 with input 8" should "be 1000" in {
    assert(DiagnosticProgram.execute(Vector(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99), List(8)).output === List(1000))
  }

  "A program 3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99 with input 7" should "be 999" in {
    assert(DiagnosticProgram.execute(Vector(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99), List(7)).output === List(999))
  }

  "The program from file with input 5" should "be 10428568" in {
    val program = Source.fromResource("DiagnosticProgram.txt").getLines.flatMap(_.toString.split(",").map(_.toInt)).toVector
    assert(DiagnosticProgram.execute(program, List(5)).output === List(10428568))
  }
}
