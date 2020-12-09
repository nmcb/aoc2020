import scala.io._
import scala.annotation.tailrec

object Day09 extends App {

  def input: List[Long] =
    Source
      .fromFile("src/resources/input09.txt")
      .getLines
      .map(_.toLong)
      .toList

  val (preamable, inbound) = input.splitAt(25)

  val answer1: Long = {
    @tailrec def answer1(todo: List[Long], prem: List[Long] = preamable.toList): Long =
      todo match {
        case Nil =>
          sys.error("boom")
        case test :: rest =>
          val sums = prem.combinations(2).map(_.sum)
          if (sums.contains(test))
            answer1(rest, prem.drop(1) :+ test)      
          else
            test
      }
    answer1(inbound.toList)
  }

  println(s"Answer part 1: ${answer1}")

  val answer2: Long = {
    @tailrec def answer2(todo: List[List[Long]], window: Int): Long = 
      todo match {
        case test :: rest if test.sum == answer1 =>
          test.max + test.min
        case _ :: rest =>
          answer2(rest, window)
        case _ =>
          answer2(inbound.sliding(window + 1).toList, window + 1)
      }
    answer2(inbound.sliding(2).toList, 2)
  }

  println(s"Answer part 2: ${answer2}")
 }
