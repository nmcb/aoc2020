import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
  
import scala.io._
import scala.annotation.tailrec

object Day08 extends App {

  val Line = """^(\w+) ([\+-]\d+)$""".r

  def parseInst(line: String): Inst =
    line match { case Line(code, arg) => Inst(code.trim, arg.toInt) }

  case class Inst(code: String, arg: Int)

  def program(file: String): List[Inst] =
    Source
      .fromFile(file)
      .getLines
      .map(parseInst)
      .toList

  case class Reg(pc: Int, acc: Int) {
    val line: Int = pc + 1
  }
  object Reg {
    def empty: Reg =
      Reg(0,0)
  }

  case class VM( prg: List[Inst]
               , reg: Reg            = Reg.empty
               , visited: Set[Int]   = Set.empty
               , halted: Boolean     = false
               , terminated: Boolean = false
               ) {

    final val run: Boolean => VM = 
      debug => if (debug && visited.contains(reg.pc))
        copy(halted = true)
      else
        prg
          .lift(reg.pc)
          .map(inst => copy(reg = step(inst), visited = visited + reg.pc).run(debug))
          .getOrElse(copy(terminated = true))

    val step: Inst => Reg =
      inst => inst.code match {
        case "nop" => reg.copy(pc = reg.pc + 1)
        case "acc" => reg.copy(pc = reg.pc + 1, acc = reg.acc + inst.arg)
        case "jmp" => reg.copy(pc = reg.pc + inst.arg)
      }
  }

  val answer1: VM =
    VM(program("src/resources/input08.txt")).run(true)
  
  println(s"Answer part 1: ${answer1.reg.acc}")

  println(s"Debug: pc=${answer1.reg.pc}, line=${answer1.reg.line} inst=${answer1.prg(answer1.reg.pc)}")
  
  val visited =
    answer1
      .visited
      .filter(pc => answer1.prg(pc).code == "nop" || answer1.prg(pc).code == "jmp")

  println(s"Debug: nops & jmps visited=${visited}")

  def hotfix(pc: Int): List[Inst] = {
    val prog = program(s"src/resources/input08.txt")
    prog(pc) match {
      case Inst("nop", arg) =>  prog.updated(pc, Inst("jmp", arg))
      case Inst("jmp", arg) =>  prog.updated(pc, Inst("nop", arg))
    }
  }    

  val possibleFixesOnLine: Map[Int,List[Inst]] =
    visited.foldLeft(Map.empty[Int,List[Inst]])((prgs,pc) => prgs + (pc + 1 -> hotfix(pc)))

  val fixedFile: Option[String] =
    possibleFixesOnLine.foldLeft(Option.empty[String]) {
      case (None, (line, attempt)) =>
        VM(attempt).run(true) match {
          case VM(_, _, _, true , false) =>
            println(s"attempt fixing line $line loops infinitely, continuing ...")
            None
          case VM(_, _, _, false, true)  =>
            println(s"attempt fixing line $line terminated!")
            println(s"hotfixing ... ")
            val fix =
              s"""${attempt
                      .map(i => s"""${i.code} ${if (i.arg >= 0) "+" + i.arg else i.arg}""")
                      .mkString("\n")}"""
            val file = "src/resources/input08hotfixed.txt"
            Files.write(Paths.get(file), fix.getBytes(StandardCharsets.UTF_8))
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
      println("hotfix available running ...")
      val answer2: VM = VM(program(file)).run(false)
      println(s"Answer part 2: ${answer2.reg.acc}")
  }  
}
