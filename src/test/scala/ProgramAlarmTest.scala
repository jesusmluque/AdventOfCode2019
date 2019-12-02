import org.scalatest.FlatSpec

import scala.io.Source

class ProgramAlarmTest extends FlatSpec {

  "A program 1,9,10,3,2,3,11,0,99,30,40,50 " should "becomes 3500,9,10,70,2,3,11,0,99,30,40,50" in {
    assert(ProgramAlarm.execute(Vector(1,9,10,3,2,3,11,0,99,30,40,50)) === Vector(3500,9,10,70,2,3,11,0,99,30,40,50))
  }

  "A program 1,0,0,0,99 " should "becomes 2,0,0,0,99" in {
    assert(ProgramAlarm.execute(Vector(1,0,0,0,99)) === Vector(2,0,0,0,99))
  }

  "A program 2,3,0,3,99 " should "becomes 2,3,0,6,99" in {
    assert(ProgramAlarm.execute(Vector(2,3,0,3,99)) === Vector(2,3,0,6,99))
  }

  "A program 2,4,4,5,99,0 " should "becomes 2,4,4,5,99,9801" in {
    assert(ProgramAlarm.execute(Vector(2,4,4,5,99,0)) === Vector(2,4,4,5,99,9801))
  }

  "A program 1,1,1,4,99,5,6,0,99 " should "becomes 30,1,1,4,2,5,6,0,99" in {
    assert(ProgramAlarm.execute(Vector(1,1,1,4,99,5,6,0,99)) === Vector(30,1,1,4,2,5,6,0,99))
  }

  "A program from file " should "get index 0 equal to  3850704" in {
    val program = Source.fromResource("ProgramAlarm.txt").getLines.flatMap(_.toString.split(",").map(_.toInt)).toVector
    val program1202 = program.updated(1, 12).updated(2, 2)
    assert(ProgramAlarm.execute(program1202)(0) === 3850704)
  }

  "A program from file " should "get index 0 equal to  19690720" in {
    val program = Source.fromResource("ProgramAlarm.txt").getLines.flatMap(_.toString.split(",").map(_.toInt)).toVector
    val program1202 = program.updated(1, 67).updated(2, 18)
    assert(ProgramAlarm.execute(program1202)(0) === 19690720)
  }
}
