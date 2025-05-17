import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
  
import scala.io._
import scala.annotation.tailrec

object Day08 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val Line = """^(\w+) ([\+-]\d+)$""".r

  def parseInst(line: String): Inst =
    line match { case Line(code, arg) => Inst(code.trim, arg.toInt) }

  case class Inst(code: String, arg: Int)

  case class Prog(instructions: List[Inst]) {

    def instruction(pc: Int): Inst =
      instructions(pc)

    override def toString: String =
      s"""${instructions
              .map(i => s"""${i.code} ${if (i.arg >= 0) "+" + i.arg else i.arg}""")
              .mkString("","\n","\n")}"""
  }

  def load(file: String): Prog =
    Prog(Source.fromFile(file).getLines.map(parseInst).toList)

  case class State(pc: Int, acc: Int) {
    val line: Int = pc + 1
  }
  object State {
    def empty: State =
      State(0,0)
  }

  case class VM( program: Prog
               , state: State        = State.empty
               , visited: Set[Int]   = Set.empty
               , halted: Boolean     = false
               , terminated: Boolean = false
               ) {

    def run(debug: Boolean = false): VM = 
      if (debug && visited.contains(state.pc))
        copy(halted = true)
      else
        program
          .instructions
          .lift(state.pc)
          .map(inst => copy(state = exec(inst), visited = visited + state.pc).run(debug))
          .getOrElse(copy(terminated = true))

    private def exec(inst: Inst): State =
      inst.code match {
        case "nop" => state.copy(pc = state.pc + 1)
        case "acc" => state.copy(pc = state.pc + 1, acc = state.acc + inst.arg)
        case "jmp" => state.copy(pc = state.pc + inst.arg)
      }
  }

  val answer1: VM = VM(load("src/resources/input08.txt")).run(debug = true)
  
  val state = answer1.state
  val prog  = answer1.program
  val trace = answer1.visited
  println(s"Answer part 1: ${state.acc}")
  println(s"Debug: pc=${state.pc}, line=${state.line} inst=${prog.instruction(state.pc)}")
  
  val visited = trace.filter(pc => prog.instruction(pc).code == "nop" || prog.instruction(pc).code == "jmp")
  println(s"Debug: nops & jmps visited=${visited}")

  def hotfix(pc: Int): Prog = {
    val program = load(s"src/resources/input08.txt")
    program.instruction(pc) match {
      case Inst("nop", arg) => Prog(program.instructions.updated(pc, Inst("jmp", arg)))
      case Inst("jmp", arg) => Prog(program.instructions.updated(pc, Inst("nop", arg)))
      case Inst(op, arg)    => sys.error(s"invalid op=$op, arg=$arg")
    }
  }    

  val possibleFixesByLine: Map[Int,Prog] =
    visited.foldLeft(Map.empty[Int,Prog])((programs,pc) => programs + (pc + 1 -> hotfix(pc)))

  val fixedFile: Option[String] =
    possibleFixesByLine.foldLeft(Option.empty[String]) {
      case (None, (line, attempt)) =>
        VM(attempt).run(debug = true) match {
          case VM(_, _, _, true , false) =>
            println(s"attempt fixing line $line loops infinitely, continuing ...")
            None
          case VM(fix, _, _, false, true)  =>
            println(s"attempt fixing line $line terminated!")
            println(s"hotfixing ... ")
            val file = "src/resources/input08hotfixed.txt"
            Files.write(Paths.get(file), fix.toString.getBytes(StandardCharsets.UTF_8))
            println(s"fix available at $file")
            Some(file)
          case _                            =>
            sys.error(s"boom: attempting to fix line $line => $attempt")
        }
      case (file: Some[String], _) =>
        file
    }

  fixedFile match {
    case None =>
      println("no hotfix available; giving up!")
    case Some(file) =>
      println("hotfix available; running ...")
      val answer2: VM = VM(load(file)).run(debug = false)
      println(s"Answer part 2: ${answer2.state.acc}")
  }
