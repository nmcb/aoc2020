import scala.io.*

object Day06 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val input: Vector[String] =
    Source.fromResource(s"input$day.txt").getLines.toVector

  val answers: Vector[Vector[String]] =
    input.foldLeft(Vector(Vector.empty[String])):
      case (result, line) if line.nonEmpty => (line +: result.head) +: result.tail
      case (result, _)                     => Vector.empty[String] +: result

  def chars(list: Vector[String]): String =
    list.fold("")(_+_).distinct
    
  val start1  = System.currentTimeMillis
  val answer1 = answers.map(chars).map(_.size).sum
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = answers.map(grp => grp.fold(chars(grp))(_ intersect _).size).sum
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

