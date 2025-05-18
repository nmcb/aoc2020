import scala.io.*

object Day08 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val Line = """^(\w+) ([\+-]\d+)$""".r


  case class Instruction(code: String, arg: Int)

  object Instruction:
    def fromLine(line: String): Instruction =
      line match
        case Line(code, arg) => Instruction(code.trim, arg.toInt)

  type Program = Vector[Instruction]

  enum State(val pc: Int, val accumulator: Int):
    case Running(override val pc: Int, override val accumulator: Int)    extends State(pc, accumulator)
    case Halted(override val pc: Int, override val accumulator: Int)     extends State(pc, accumulator)
    case Terminated(override val pc: Int, override val accumulator: Int) extends State(pc, accumulator)

    def isRunning: Boolean    = this.isInstanceOf[Running]
    def isHalted: Boolean     = this.isInstanceOf[Halted]
    def isTerminated: Boolean = this.isInstanceOf[Terminated]

    def setRunning: Running       = Running(pc, accumulator)
    def setHalted: Halted         = Halted(pc, accumulator)
    def setTerminated: Terminated = Terminated(pc, accumulator)

  import State.*

  case class VM(program: Program, state: State = Running(pc = 0, accumulator = 0), trace: Vector[Int] = Vector.empty):

    def run(debug: Boolean): VM =
      if (debug && trace.contains(state.pc))
        copy(state = state.setHalted)
      else
        program
          .lift(state.pc)
          .map(instruction => copy(state = exec(instruction), trace = trace :+ state.pc).run(debug))
          .getOrElse(copy(state = state.setTerminated))

    private def exec(inst: Instruction): State =
      inst.code match
        case "nop" => Running(state.pc + 1, state.accumulator)
        case "acc" => Running(state.pc + 1, state.accumulator + inst.arg)
        case "jmp" => Running(state.pc + inst.arg, state.accumulator)

  val program: Program =
    Source
      .fromResource(s"input$day.txt")
      .getLines.map(Instruction.fromLine)
      .toVector

  val start1  = System.currentTimeMillis
  val answer1 = VM(program).run(debug = true).state.accumulator
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def patch(program: Program): Program =

    def patchLine(line: Int): Program =
      program(line) match
        case Instruction("nop", arg) => program.updated(line, Instruction("jmp", arg))
        case Instruction("jmp", arg) => program.updated(line, Instruction("nop", arg))
        case Instruction(op, arg) => sys.error(s"invalid op=$op, arg=$arg")

    VM(program).run(debug = true)
      .trace
      .filter(line => program(line).code == "nop" || program(line).code == "jmp")
      .foldLeft(Set.empty[Program])((patches,line) => patches + patchLine(line))
      .find(patch => VM(patch).run(debug = true).state.isTerminated)
      .get

  val start2 = System.currentTimeMillis
  val answer2 = VM(patch(program)).run(debug = false).state.accumulator
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
