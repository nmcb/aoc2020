import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
  
import scala.io._
import scala.annotation.tailrec

object Day08 extends App {

  val Line = """^(\w+) ([\+-]\d+)$""".r

  def parseInst(line: String): Inst =
    line match { case Line(code, arg) => Inst(code.trim, arg.toInt) }

  case class Inst(code: String, arg: Int, didRun: Boolean = false)

  def program(file: String): List[Inst] =
    Source
      .fromFile(file)
      .getLines
      .map(parseInst)
      .toList

  case class VM( prg: List[Inst]
               , pc: Int             = 0
               , acc: Int            = 0
               , visited: Set[Int]   = Set.empty
               , halted: Boolean     = false
               , terminated: Boolean = false
               ) {

    @tailrec final def run(debug: Boolean = false): VM = 
      if (debug && visited.contains(pc))
        copy(halted = true)
      else {
        prg.lift(pc) match {
          case None =>
            copy(terminated = true)
          case Some(inst) =>
            val (pc1,acc1) = interpret(inst)
            copy(pc = pc1, acc = acc1, visited = visited + pc).run(debug)
          }
      }

    private def interpret(inst: Inst): (Int,Int) =
      inst.code match {
        case "nop" => (pc + 1        , acc           )
        case "acc" => (pc + 1        , acc + inst.arg)
        case "jmp" => (pc + inst.arg , acc           )
      }
  }

  val answer1: VM =
    VM(program("src/resources/input08.txt")).run(debug = true)
  
  println(s"Answer part 1: ${answer1.acc}")

  println(s"Debug: pc=${answer1.pc}, line=${answer1.pc + 1} inst=${answer1.prg(answer1.pc)}")
  
  val visited =
    answer1
      .visited
      .filter(pc => answer1.prg(pc).code == "nop" || answer1.prg(pc).code == "jmp")

  println(s"Debug: nops & jmps visited=${visited}")

  def hotfix(pc: Int): List[Inst] = {
    val prog = program(s"src/resources/input08.txt")
    prog(pc) match {
      case Inst("nop", arg, didRun) =>  prog.updated(pc, Inst("jmp", arg, didRun))
      case Inst("jmp", arg, didRun) =>  prog.updated(pc, Inst("nop", arg, didRun))
    }
  }    

  val possibleFixes: Map[Int,List[Inst]] =
    visited.foldLeft(Map.empty[Int,List[Inst]])((prgs,pc) => prgs + (pc + 1 -> hotfix(pc)))

  val fixedFile: Option[String] =
    possibleFixes.foldLeft(Option.empty[String]) {
      case (None, (line, attempt)) =>
        VM(attempt).run(debug = true) match {
          case VM(_, _, _, _, true , false) =>
            println(s"attemt fixing line $line loops infinitely, continuing ...")
            None
          case VM(_, _, _, _, false, true)  =>
            println(s"attempt terminated! line $line was in error")
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
            sys.error(s"boom attempting to fix line $line => $attempt")
        }
      case (file: Some[String], _) =>
        file
    }

  fixedFile match {
    case None =>
      println("no hotfix available; giving up!")
    case Some(file) =>
      println("hotfix available running ...")
      val answer2: VM = VM(program("src/resources/input08hotfixed.txt")).run(debug = false)
      println(s"Answer part 2: ${answer2.acc}")
  }  
}
