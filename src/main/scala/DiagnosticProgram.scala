object DiagnosticProgram {

  case class Status(state: Vector[Int], output: List[Int], pointer: Int, status: String)

  def execute(instructions: Vector[Int], input: List[Int], pointer:Int = 0): Status = {

    def executeWithPointer(pointer: Int, instructions: Vector[Int], outputs: Status, input: List[Int]): Status = {
      def calculateValues(params: List[Int]) = {
        (pointer + 1 to pointer + params.length).zipWithIndex.map {
          case (idx, i) if params(i) == 0 => instructions(instructions(idx))
          case (idx, i) if params(i) == 1 => instructions(idx)
        }
      }

      def completeParams(p: List[Int]) = {
        val params = if (p.length < 2) p ++ List(0) else p
        params
      }

      val itemp = instructions(pointer).toString.split("").map(_.toInt).reverse.toList
      val ins = if (itemp.length == 1) itemp(0) :: List(0,0) else itemp

      if (pointer >= instructions.size)
        Status(outputs.state, outputs.output, pointer, "Error")
      else if (ins.head == 3 && input.isEmpty)
        Status(outputs.state, outputs.output, pointer, "Waiting")
      else {
        ins match {
          case 1 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val newInstructions = instructions.updated(instructions(pointer + 1 + params.length), calculateValues(params).sum)
            executeWithPointer(pointer + 2 + params.length,
              newInstructions, Status(newInstructions, outputs.output, pointer + 2 + params.length, "running"), input)
          }
          case 2 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val newInstructions = instructions.updated(instructions(pointer + 1 + params.length), calculateValues(params).product)
            executeWithPointer(pointer + 2 + params.length,
              newInstructions, Status(newInstructions, outputs.output, pointer + 2 + params.length, "running"), input)
          }
          case 3 :: List(0,0) => {
            val newInstructions = instructions.updated(instructions(pointer + 1), input.head)
            executeWithPointer(pointer + 2, newInstructions, Status(newInstructions, outputs.output, pointer + 2, "running"), input.tail)
          }
          case 4 :: 0 :: params => {
            executeWithPointer(pointer  + 1 + params.length, instructions,
              params match {
                case List(1) => Status(instructions, instructions(pointer + 1) :: outputs.output, pointer, "running")
                case List(0) => Status(instructions, instructions(instructions(pointer + 1)) :: outputs.output, pointer  + 1 + params.length, "running")
              }, input
            )
          }
          case 5 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            val newPointer = if (values.head != 0) values(1) else pointer + 3
            executeWithPointer(newPointer,
              instructions, Status(instructions, outputs.output, newPointer, "running"), input)

          }
          case 6 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            val newPointer = if (values.head == 0) values(1) else pointer + 3
            executeWithPointer(newPointer,
              instructions, Status(instructions, outputs.output, pointer, "running"), input)

          }
          case 7 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            val newInstructions = if (values.head < values(1)) instructions.updated(instructions(pointer + 3), 1) else instructions.updated(instructions(pointer + 3), 0)
            executeWithPointer(pointer + 4,
              newInstructions, Status(newInstructions, outputs.output, pointer + 4, "running"), input)
          }
          case 8 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            val newInstructions = if (values.head == values(1)) instructions.updated(instructions(pointer + 3), 1) else instructions.updated(instructions(pointer + 3), 0)
            executeWithPointer(pointer + 4,
              newInstructions, Status(newInstructions, outputs.output, pointer + 4, "running"), input)
          }

          case List(9, 9) => {
            Status(instructions, outputs.output, pointer, "Halt")
          }
        }
      }

    }
    executeWithPointer(pointer, instructions, Status(instructions, List(), 0, "Init"), input)
  }

}
