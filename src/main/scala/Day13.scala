import scala.io._
import scala.annotation._
import scala.util.control.Breaks._
import scala.util._
import scala.collection._

object Day13 extends App {

  val List(begin, input) =
    Source
      .fromFile("src/resources/input13.txt")
      .getLines
      .toList

  val schedule: List[(Long,Int)] =
    input
      .split(',')
      .zipWithIndex
      .foldLeft(List.empty) {
        case (acc,(bus,offset)) =>
          Try(bus.toLong).map(b => acc ++ Map(b -> offset)).getOrElse(acc)
      }

  def depart(at: Long)(bus: Long): Boolean =
    at % bus == 0
    
  val start1 = System.currentTimeMillis

  val answer1: Long = {
    val starttime = begin.toLong
    @tailrec def answer1(time: Long): Long =
      schedule.toMap.keys.find(depart(time)) match {
        case None      => answer1(time + 1)
        case Some(bus) => (time - starttime) * bus
      }
    answer1(starttime)
  }

  println(s"Answer part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2 = System.currentTimeMillis

  def congruence(n: List[Long], a: List[Long]): Option[Long] = {
    require(n.size == a.size)
    val prod = n.product
 
    def iter(n: List[Long], a: List[Long], sm: Long): Long = {
      def gcd(a: Long, b: Long): Long = {
        def loop(a: Long, b: Long, x0: Long, x1: Long): Long = {
          if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1
        }
 
        if (b == 1) 1 else {
          val x1 = loop(a, b, 0, 1)
          if (x1 < 0) x1 + b else x1
        }
      }
 
      if (n.isEmpty) sm else {
        val p = prod / n.head
        iter(n.tail, a.tail, sm + a.head * gcd(p, n.head) * p)
      }
    }
 
    Try(iter(n, a, 0) % prod) match {
      case Success(v) => Some(v)
      case _          => None
    }
  }

  val schedule2: Map[Long,Long] =
    input
      .split(',')
      .zip((0 until input.length).map(_.toLong).reverse)
      .map((b,i) => (Try(b.toLong).getOrElse[Long](1),i))
      .toMap

  val answer2 =
    congruence(schedule2.keys.toList, schedule2.values.toList).getOrElse(sys.error("boom")) - input.length + 1

  println(s"Answer part 2: ${answer2} [${System.currentTimeMillis - start1}ms]")
}