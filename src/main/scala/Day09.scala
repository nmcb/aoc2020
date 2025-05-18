import scala.io._
import scala.annotation.tailrec
import scala.util.control.Breaks._

object Day09 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  def input: Vector[Long] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.toLong)
      .toVector

  val (preamble, inbound) = input.splitAt(25)

  def solve1(preamble: Vector[Long], inbound: Vector[Long]): Long =
    @tailrec def go(todo: Vector[Long], prem: Vector[Long] = preamble): Long =
      todo match
        case test +: rest =>
          if prem.combinations(2).map(_.sum).contains(test) then
            go(rest, prem.drop(1) :+ test)
          else
            test
        case _            => sys.error("boom")
    go(inbound)

  val start1 = System.currentTimeMillis
  val answer1 = solve1(preamble, inbound)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def solve2(inbound: Vector[Long], sum: Long): Option[Long] =
    var result: Option[Long] = None
    breakable {
      for size <- 2 to inbound.length do
        for test <- inbound.sliding(size) do
          if test.sum == sum then
            result = Some(test.max + test.min)
            break
        if result.isDefined then
          break
    }
    result

  val start2 = System.currentTimeMillis
  val answer2: Long = solve2(inbound, answer1).get
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
