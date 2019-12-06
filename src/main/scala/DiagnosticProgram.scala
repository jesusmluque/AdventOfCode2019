object DiagnosticProgram {

  def execute(instructions: Vector[Int], input: Int): List[Int] = {

    def executeWithPointer(pointer: Int, instructions: Vector[Int], outputs: List[Int]): List[Int] = {
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

      if (pointer >= instructions.size)
        outputs
      else {
        val itemp = instructions(pointer).toString.toString.split("").map(_.toInt).reverse.toList
        val ins = if (itemp.length == 1) itemp(0) :: List(0,0) else itemp

        ins match {
          case 1 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            executeWithPointer(pointer + 2 + params.length,
              instructions.updated(instructions(pointer + 1 + params.length), calculateValues(params).sum), outputs)
          }
          case 2 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            executeWithPointer(pointer + 2 + params.length,
              instructions.updated(instructions(pointer + 1 + params.length), calculateValues(params).product), outputs)
          }
          case 3 :: List(0,0) => executeWithPointer(pointer + 2, instructions.updated(instructions(pointer + 1), input), outputs)
          case 4 :: 0 :: params => {
            executeWithPointer(pointer  + 1 + params.length, instructions,
              params match {
                case List(1) => instructions(pointer + 1) :: outputs
                case List(0) => instructions(instructions(pointer + 1)) :: outputs
              }
            )
          }
          case 5 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            executeWithPointer(if (values.head != 0) values(1) else pointer + 3,
              instructions, outputs)

          }
          case 6 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            executeWithPointer(if (values.head == 0) values(1) else pointer + 3,
              instructions, outputs)

          }
          case 7 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            executeWithPointer(pointer + 4,
              if (values.head < values(1)) instructions.updated(instructions(pointer + 3), 1) else instructions.updated(instructions(pointer + 3), 0), outputs)
          }
          case 8 :: 0 :: p => {
            val params: List[Int] = completeParams(p)
            val values = calculateValues(params)
            executeWithPointer(pointer + 4,
              if (values.head == values(1)) instructions.updated(instructions(pointer + 3), 1) else instructions.updated(instructions(pointer + 3), 0), outputs)
          }

          case List(9, 9) => outputs
        }
      }

    }
    executeWithPointer(0, instructions, List())
  }

}
