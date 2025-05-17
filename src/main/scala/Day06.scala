import scala.io._

object Day06 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val input: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList

  val answers: List[List[String]] =
    input.foldLeft(List(List.empty[String])) {
      case (acc, line) if line.nonEmpty => (line :: acc.head) :: acc.tail
      case (acc, _) => List.empty[String] :: acc
    }

  def chars(list: List[String]): String =
    list
      .fold("")(_+_)
      .distinct
    
  val answer1: Int =
    answers
      .map(chars)
      .map(_.size)
      .sum

  val answer2: Int =
    answers
      .map(grp => grp.fold(chars(grp))(_ intersect _).size)
      .sum

  println(s"Answer part 1: ${answer1}")
  println(s"Answer part 2: ${answer2}")

