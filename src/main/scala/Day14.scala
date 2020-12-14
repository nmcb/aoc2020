import scala.io._
import scala.annotation._
import scala.util.control.Breaks._
import scala.util._
import scala.collection._

object Day14 extends App {

  case class UInt36(underlying: Long) {
    assert(underlying.toBinaryString.length <= 36)

    def this(string: String, radix: Int) = 
      this(BigInt(string, radix).longValue)

    def toBinaryString: String = 
      List.fill(36 - underlying.toBinaryString.length)('0').mkString + underlying.toBinaryString

    override def toString: String =
      underlying.toString
  }

  val MaskExpr   = "mask = (.+)".r
  val UpdateExpr = "mem\\[(\\d+)\\] = (\\d+)".r

  sealed trait Inst

  case class Mask(underlying: String) extends Inst {
    assert(underlying.filter(c => c == 'X' || c == '1' || c == '0').length == 36)

    def mask(value: UInt36): UInt36 =
      new UInt36(
        underlying
          .zip(value.toBinaryString)
          .foldRight("") {
            case (('X',b), lsb) => "" +  b  + lsb
            case (('1',_), lsb) => "" + '1' + lsb
            case (('0',_), lsb) => "" + '0' + lsb
            case err => sys.error(s"boom: $err")
          }, 2)

    def float(address: UInt36): List[UInt36] =
      underlying
        .zip(address.toBinaryString)
        .foldRight("") {
          case (('X',_),lsb) => "" + 'X' + lsb
          case (('1',_),lsb) => "" + '1' + lsb
          case (('0',b),lsb) => "" +  b  + lsb
        }.foldRight(List.empty[String]) {
          case ('X',acc) => if acc.nonEmpty then acc.flatMap(n => List("0" + n, "1" + n)) else List("0", "1")
          case ('1',acc) => if acc.nonEmpty then acc.map(n => "1" + n) else List("1")
          case ('0',acc) => if acc.nonEmpty then acc.map(n => "0" + n) else List("0")
          case err => sys.error(s"boom : $err") 
        }.map(str => new UInt36(str, 2))

    override def toString: String =
      underlying
  }

  object Mask {
    def fill(c: Char): Mask = Mask(List.fill(36)(c).mkString)
  }

  case class Update(addr: UInt36, value: UInt36) extends Inst

  type Mem = Map[UInt36,UInt36]
  type Prg = List[Inst]

  val program: Prg =
    Source
      .fromFile("src/resources/input14.txt")
      .getLines
      .toList
      .map {
        case UpdateExpr(addr,value) => Update(new UInt36(addr, 10), new UInt36(value, 10))
        case MaskExpr(mask)         => Mask(mask)
      }

  def run(program: Prg)(codec: UInt36 => UInt36 => Mask => Mem => Mem): Long = {
    def loop(prog: Prg, mem: Mem = Map.empty, mask: Mask = Mask.fill('X')) : Mem =
      prog match {
        case Nil          => mem
        case inst :: rest => inst match {
            case Update(addr,value) => loop(rest, codec(addr)(value)(mask)(mem), mask)
            case m: Mask          => loop(rest, mem , m)
        }
      }
    loop(program).values.map(_.underlying).sum
  }   
  
  val start1  = System.currentTimeMillis
  val answer1 = run(program)(addr => value => mask => mem => mem ++ Map(addr -> mask.mask(value)))
  println(s"Answer part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = run(program)(addr => value => mask => mem => mem ++ mask.float(addr).map(a => a -> value))
  println(s"Answer part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
}