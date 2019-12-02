import scala.annotation.tailrec

object ProgramAlarm {

  def execute(program: Vector[Int]):Vector[Int] = {
    @tailrec
    def executeWithIndex(index: Int, program: Vector[Int]):Vector[Int] = {
      if (index >= program.size)
        program
      else
        program(index) match {
          case 1 => executeWithIndex(index + 4, program.updated(program(index + 3), program(program(index + 1)) + program(program(index + 2))))
          case 2 => executeWithIndex(index + 4, program.updated(program(index + 3), program(program(index + 1)) * program(program(index + 2))))
          case 99 => program
        }
    }
    executeWithIndex(0, program)
  }


}