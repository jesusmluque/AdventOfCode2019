object DiagnosticProgram {

  case class Status(state: Vector[Long], output: List[Long], pointer: Int, status: String)

  def execute(instructions: Vector[Long], input: List[Int], pointer:Int = 0, relativeBase:  Int = 0): Status = {

    def executeWithPointer(pointer: Int, instructions: Vector[Long], outputs: Status, input: List[Int], relativeBase: Int): Status = {
      def calculateValues(params: List[Int]) = {
        (pointer + 1 to pointer + params.length).zipWithIndex.map {
          case (idx, i) if params(i) == 0 => readInstructions(instructions, readInstructions(instructions, idx)._2.toInt)._2
          case (idx, i) if params(i) == 1 => readInstructions(instructions, idx)._2
          case (idx, i) if params(i) == 2 => readInstructions(instructions, readInstructions(instructions, idx)._2.toInt + relativeBase)._2
        }
      }

      def completeParams(p: List[Int]) = {
        val params = if (p.length < 2) p ++ List(0) else p
        params
      }

      def updateInstructions(instructions: Vector[Long], index: Int, value: Long) = {
        if (index < instructions.length)
          instructions.updated(index, value)
        else
          (instructions ++ Vector.fill(index  + 1 - instructions.length)(0L)).updated(index, value)
      }

      def readInstructions(instructions: Vector[Long], index: Int): (Vector[Long], Long)=
        if (index < instructions.length)
          (instructions, instructions(index))
        else {
          val newIns = instructions ++ Vector.fill(index + 1 - instructions.length)(0L)
          (newIns, newIns(index))
        }


      val itemp = instructions(pointer).toString.split("").map(_.toInt).reverse.toList
      val ins = if (itemp.length == 1) itemp(0) :: List(0,0) else itemp
      if (ins == List(3, 0, 2))
        System.out.println(pointer, ins,  outputs.output)
      if (ins.head == 3 && input.isEmpty)
        Status(outputs.state, outputs.output, pointer, "Waiting")
      else {
        ins match {
          case 1 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val newInstructions = updateInstructions(instructions, readInstructions(instructions, pointer + 1 + params.length)._2.toInt, calculateValues(params).sum)
            executeWithPointer(pointer + 2 + params.length,
              newInstructions, Status(newInstructions, outputs.output, pointer + 2 + params.length, "running"), input, relativeBase = relativeBase)
          }
          case 2 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val newInstructions = updateInstructions(instructions, readInstructions(instructions, pointer + 1 + params.length)._2.toInt, calculateValues(params).product)
            executeWithPointer(pointer + 2 + params.length,
              newInstructions, Status(newInstructions, outputs.output, pointer + 2 + params.length, "running"), input, relativeBase = relativeBase)
          }
          case 3 :: 0 :: p => {
            val index = p match {
              case List(0) => readInstructions(instructions, pointer + 1)._2.toInt
              case List(2) => readInstructions(instructions, pointer + 1)._2.toInt + relativeBase
            }
            val newInstructions = updateInstructions(instructions, index, input.head)
            executeWithPointer(pointer + 2, newInstructions, Status(newInstructions, outputs.output, pointer + 2, "running"), input.tail, relativeBase = relativeBase)
          }
          case 4 :: 0 :: params => {
            executeWithPointer(pointer  + 1 + params.length, instructions,
              params match {
                case List(1) => Status(instructions, readInstructions(instructions, pointer + 1)._2 :: outputs.output, pointer, "running")
                case List(0) => Status(instructions, readInstructions(instructions, readInstructions(instructions, pointer + 1)._2.toInt)._2 :: outputs.output, pointer  + 1 + params.length, "running")
                case List(2) => Status(instructions, readInstructions(instructions, readInstructions(instructions, pointer + 1)._2.toInt + relativeBase)._2 :: outputs.output, pointer  + 1 + params.length, "running")
              }, input, relativeBase = relativeBase
            )
          }
          case 5 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            val newPointer = (if (values.head != 0) values(1) else pointer + 3).toInt
            executeWithPointer(newPointer,
              instructions, Status(instructions, outputs.output, newPointer, "running"), input, relativeBase = relativeBase)

          }
          case 6 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            val newPointer = (if (values.head == 0) values(1) else pointer + 3).toInt
            executeWithPointer(newPointer,
              instructions, Status(instructions, outputs.output, pointer, "running"), input, relativeBase = relativeBase)

          }
          case 7 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            val index = if (values.length == 3) values(2).toInt else readInstructions(instructions, pointer + 3)._2.toInt
            val newInstructions = if (values.head < values(1)) updateInstructions(instructions, index, 1) else updateInstructions(instructions, index, 0)
            executeWithPointer(pointer + 4,
              newInstructions, Status(newInstructions, outputs.output, pointer + 4, "running"), input, relativeBase = relativeBase)
          }
          case 8 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            val newInstructions = if (values.head == values(1)) updateInstructions(instructions,  readInstructions(instructions, pointer + 3)._2.toInt, 1) else updateInstructions(instructions, readInstructions(instructions,  pointer + 3)._2.toInt, 0)
            executeWithPointer(pointer + 4,
              newInstructions, Status(newInstructions, outputs.output, pointer + 4, "running"), input, relativeBase = relativeBase)
          }
          case 9 :: 0 :: p => {
            val values = calculateValues(p)
            val newPointer = pointer + 2
            executeWithPointer(newPointer,
              instructions, Status(instructions, outputs.output, newPointer, "running"), input, relativeBase = relativeBase + values.head.toInt)
          }

          case List(9, 9) => {
            Status(instructions, outputs.output, pointer, "Halt")
          }
        }
      }

    }
    executeWithPointer(pointer, instructions, Status(instructions, List(), 0, "Init"), input, relativeBase = relativeBase)
  }

}
