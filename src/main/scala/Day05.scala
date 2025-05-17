import scala.io._

object Day05 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  type Id = Int

  import Integer._

  def seatId(s: String): Id =
    val row = parseInt(s.substring(0, 7).replaceAll("F","0").replaceAll("B","1"), 2)
    val col = parseInt(s.substring(7,10).replaceAll("L","0").replaceAll("R","1"), 2)
    row * 8 + col

  val seatIds: Set[Id] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(seatId)
      .toSet

  val start1  = System.currentTimeMillis
  val answer1 = seatIds.max
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def missing(seatIds: Set[Id]): Set[Id] =
    (seatIds.min to seatIds.max).toSet -- seatIds

  val start2  = System.currentTimeMillis
  val answer2 = missing(seatIds).head
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
