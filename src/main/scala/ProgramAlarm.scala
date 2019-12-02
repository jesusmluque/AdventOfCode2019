import scala.annotation.tailrec

object ProgramAlarm {

  def execute(program: Vector[Int]):Either[String, Vector[Int]] = {
    @tailrec
    def executeWithIndex(index: Int, program: Vector[Int]):Either[String, Vector[Int]] = {
      if (index >= program.size)
        Right(program)
      else
        program(index) match {
          case 1 => executeWithIndex(index + 4, program.updated(program(index + 3), program(program(index + 1)) + program(program(index + 2))))
          case 2 => executeWithIndex(index + 4, program.updated(program(index + 3), program(program(index + 1)) * program(program(index + 2))))
          case 99 => Right(program)
          case _ => Left("Error")
        }
    }
    executeWithIndex(0, program)
  }


}