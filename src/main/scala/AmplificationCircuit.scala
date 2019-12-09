object AmplificationCircuit {
  def executeStream(program: Vector[Int])(sequence: List[Int]): Int = {
    sequence.foldLeft(List(0)) { (acc, phase) =>
      DiagnosticProgram.execute(program, phase :: acc).output
    }
  }.head

  def executeFeedbackLoop(program: Vector[Int])(sequence: List[Int]) = {
    def loop(input: Int, programs: List[DiagnosticProgram.Status]): Int = {
      if (programs.last.status != "Halt") {
        val ps = programs.foldLeft((input, List[DiagnosticProgram.Status]())) { (acc, p) =>
          val next = DiagnosticProgram.execute(p.state, List(acc._1), p.pointer)
          (next.output.head, DiagnosticProgram.Status(next.state, next.output, next.pointer, next.status) :: acc._2)
        }
        loop(ps._1, ps._2.reverse)
      }
      else
        input
    }
    val a = DiagnosticProgram.execute(program, List(sequence(0), 0))
    val b = DiagnosticProgram.execute(program, List(sequence(1), a.output.head))
    val c = DiagnosticProgram.execute(program, List(sequence(2), b.output.head))
    val d = DiagnosticProgram.execute(program, List(sequence(3), c.output.head))
    val e = DiagnosticProgram.execute(program, List(sequence(4), d.output.head))

    val programs = List(a, b, c, d, e)
    loop(e.output.head, programs)

  }

  def findMax(program: Vector[Int]) =
    permutations(List(0,1,2,3,4)).map(executeStream(program)).max

  def findMaxWithLoop(program: Vector[Int]) = {
    val res = permutations(List(5,6,7,8,9)).map(executeFeedbackLoop(program))
    System.out.println(res)
    res.max
  }


  def permutations[T](lst: List[T]): List[List[T]] = lst match {
    case Nil => List(Nil)
    case x :: xs => permutations(xs) flatMap { perm =>
      (0 to xs.length) map { num =>
        (perm take num) ++ List(x) ++ (perm drop num)
      }
    }
  }
}
