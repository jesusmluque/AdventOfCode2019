object DiagnosticProgram {

  val RUNNING = "running"
  val HALT = "Halt"
  val INIT = "Init"
  val WAITING = "Waiting"

  case class Status(state: Vector[Long], output: List[Long], pointer: Int, status: String)


  def execute(instructions: Vector[Long], input: List[Int], pointer:Int = 0, relativeBase:  Int = 0): Status = {

    def executeWithPointer(pointer: Int, instructions: Vector[Long], outputs: Status, input: List[Int], relativeBase: Int): Status = {

      def calculateValues(params: List[Int]) = {
        val arguments = if (params.length == 3) params.take(2) else params
        val resArg = if (params.length == 3) params.last else 0
        ((pointer + 1 to pointer + arguments.length).zipWithIndex.map {
          case (idx, i) if params(i) == 0 => readInstructions(instructions, readInstructions(instructions, idx).toInt)
          case (idx, i) if params(i) == 1 => readInstructions(instructions, idx)
          case (idx, i) if params(i) == 2 => readInstructions(instructions, readInstructions(instructions, idx).toInt + relativeBase)
        }, resArg match {
          case 0 => readInstructions(instructions, pointer + arguments.length + 1).toInt
          case 2 => readInstructions(instructions, pointer + arguments.length + 1).toInt + relativeBase
        })
      }

      def completeParams(p: List[Int]) = if (p.length < 2) p ++ List(0) else p

      def updateInstructions(instructions: Vector[Long], index: Int, value: Long) =
        if (index < instructions.length)
          instructions.updated(index, value)
        else
          (instructions ++ Vector.fill(index  + 1 - instructions.length)(0L)).updated(index, value)

      def readInstructions(instructions: Vector[Long], index: Int): Long=
        if (index < instructions.length)
          instructions(index)
        else
          (instructions ++ Vector.fill(index + 1 - instructions.length)(0L)).apply(index)

      val itemp = instructions(pointer).toString.split("").map(_.toInt).reverse.toList
      val ins = if (itemp.length == 1) itemp(0) :: List(0,0) else itemp

      if (ins.head == 3 && input.isEmpty)
        Status(outputs.state, outputs.output, pointer, WAITING)
      else {

        ins match {
          case 1 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            val newInstructions = updateInstructions(instructions, values._2, values._1.sum)
            executeWithPointer(pointer + 4,
              newInstructions, Status(newInstructions, outputs.output, pointer + 4, RUNNING), input, relativeBase = relativeBase)
          }
          case 2 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            val newInstructions = updateInstructions(instructions, values._2, values._1.product)
            executeWithPointer(pointer + 4,
              newInstructions, Status(newInstructions, outputs.output, pointer + 4, RUNNING), input, relativeBase = relativeBase)
          }
          case 3 :: 0 :: p => {
            val index = p match {
              case List(0) => readInstructions(instructions, pointer + 1).toInt
              case List(2) => readInstructions(instructions, pointer + 1).toInt + relativeBase
            }
            val newInstructions = updateInstructions(instructions, index, input.head)
            executeWithPointer(pointer + 2, newInstructions, Status(newInstructions, outputs.output, pointer + 2, RUNNING), input.tail, relativeBase = relativeBase)
          }
          case 4 :: 0 :: params => {
            executeWithPointer(pointer  + 1 + params.length, instructions,
              params match {
                case List(1) => Status(instructions, readInstructions(instructions, pointer + 1) :: outputs.output, pointer, RUNNING)
                case List(0) => Status(instructions, readInstructions(instructions, readInstructions(instructions, pointer + 1).toInt) :: outputs.output, pointer  + 1 + params.length, RUNNING)
                case List(2) => Status(instructions, readInstructions(instructions, readInstructions(instructions, pointer + 1).toInt + relativeBase) :: outputs.output, pointer  + 1 + params.length, RUNNING)
              }, input, relativeBase = relativeBase
            )
          }
          case 5 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            val newPointer = (if (values._1.head != 0) values._1(1) else pointer + 3).toInt
            executeWithPointer(newPointer,
              instructions, Status(instructions, outputs.output, newPointer, RUNNING), input, relativeBase = relativeBase)

          }
          case 6 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            val newPointer = (if (values._1.head == 0) values._1(1) else pointer + 3).toInt
            executeWithPointer(newPointer,
              instructions, Status(instructions, outputs.output, newPointer, RUNNING), input, relativeBase = relativeBase)

          }
          case 7 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            val index = values._2
            val newInstructions = if (values._1.head < values._1(1)) updateInstructions(instructions, index, 1) else updateInstructions(instructions, index, 0)
            executeWithPointer(pointer + 4,
              newInstructions, Status(newInstructions, outputs.output, pointer + 4, RUNNING), input, relativeBase = relativeBase)
          }
          case 8 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            val index = values._2
            val newInstructions = if (values._1.head == values._1(1)) updateInstructions(instructions,  index, 1) else updateInstructions(instructions, index, 0)
            executeWithPointer(pointer + 4,
              newInstructions, Status(newInstructions, outputs.output, pointer + 4, RUNNING), input, relativeBase = relativeBase)
          }
          case 9 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            val newPointer = pointer + 2
            executeWithPointer(newPointer,
              instructions, Status(instructions, outputs.output, newPointer, RUNNING), input, relativeBase = relativeBase + values._1.head.toInt)
          }

          case List(9, 9) => {
            Status(instructions, outputs.output, pointer, HALT)
          }


        }
      }

    }

    executeWithPointer(pointer, instructions, Status(instructions, List(), 0, INIT), input, relativeBase = relativeBase)
  }

}
