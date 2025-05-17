import scala.io._
import scala.annotation.tailrec
import scala.util.control.Breaks._

object Day09 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  def input: List[Long] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.toLong)
      .toList

  val (preamable, inbound) = input.splitAt(25)

  val answer1: Long = {
    @tailrec def answer1(todo: List[Long], prem: List[Long] = preamable): Long =
      todo match {
        case test :: rest =>
          val sums = prem.combinations(2).map(_.sum)
          if (sums.contains(test))
            answer1(rest, prem.drop(1) :+ test)      
          else
            test  
        case _ =>
          sys.error("boom")
      }
    answer1(inbound.toList)
  }

  println(s"Answer part 1: ${answer1}")

  val start = System.currentTimeMillis

  // val answer2: Long = {
  //   @tailrec def answer2(todo: Iterator[List[Long]] = inbound.sliding(2), window: Int = 2): Long = 
  //     if (todo.hasNext) {
  //       val test = todo.next()
  //       if (test.sum == answer1)
  //         test.max + test.min
  //       else
  //         answer2(todo, window)
  //     }
  //     else {
  //       val size = window + 1
  //       answer2(inbound.sliding(size), size)
  //     }
  //   answer2()
  // }

  val answer2: Option[Long] = {
    var result: Option[Long] = None
    breakable {
      for (size <- 2 to inbound.length) {
        for (test <- inbound.sliding(size)) {
          if (test.sum == answer1) {
            result = Some(test.max + test.min)
            break
          }
        }
        if (result.isDefined) break
      }
    }
    result
  }  

  println(s"Answer part 2: ${answer2}")
  println(s"[${System.currentTimeMillis - start}ms]")

