import scala.io.*
import scala.annotation.*
import scala.util.*

object Day13 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val List(begin, input) =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList

  val schedule: List[(Long,Int)] =
    input
      .split(',')
      .zipWithIndex
      .foldLeft(List.empty):
        case (result,(bus,offset)) =>
          Try(bus.toLong).map(b => result ++ Map(b -> offset)).getOrElse(result)

  def depart(at: Long)(bus: Long): Boolean = at % bus == 0

  def solve1(schedule: List[(Long,Int)], start: Long): Long =
    @tailrec
    def go(time: Long): Long =
      schedule.toMap.keys.find(depart(time)) match
        case None      => go(time + 1)
        case Some(bus) => (time - start) * bus
    go(start)

  val start1 = System.currentTimeMillis
  val answer1: Long = solve1(schedule, begin.toLong)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  /**
   * Chinese Remainder Theorem, allows you to solve equations of type:
   *
   * x % n[0] == a[0]
   * x % n[1] == a[1]
   * x % n[2] == a[2]
   * ...
   *
   * @return x
   */
  def crt(n: List[Long], a: List[Long]): Option[Long] =

    require(n.size == a.size)
    val prod = n.product

    @tailrec
    def go(n: List[Long], a: List[Long], sm: Long): Long =
      def gcd(a: Long, b: Long): Long =
        def loop(a: Long, b: Long, x0: Long, x1: Long): Long =
          if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1

        if b == 1 then
          1
        else
          val x1 = loop(a, b, 0, 1)
          if x1 < 0 then x1 + b else x1

      if n.isEmpty then sm else
        val p = prod / n.head
        go(n.tail, a.tail, sm + a.head * gcd(p, n.head) * p)
 
    Try(go(n, a, 0) % prod) match
      case Success(v) => Some(v)
      case _          => None

  val equations: Map[Long,Long] =
    input
      .split(',')
      .zip((0 until input.length).map(_.toLong).reverse)
      .map((b,i) => (Try(b.toLong).getOrElse[Long](1),i))
      .toMap

  val start2 = System.currentTimeMillis
  val answer2 = crt(equations.keys.toList, equations.values.toList).getOrElse(sys.error("boom")) - input.length + 1
  println(s"Answer part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
