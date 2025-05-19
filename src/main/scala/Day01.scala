import scala.io.*

object Day01 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val input: Vector[Int] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.toInt)
      .toVector

  def answer(numberOfArgs: Int, sumEquals: Int): Int =
    input
      .combinations(numberOfArgs)
      .filter(_.sum == sumEquals)
      .map(_.product)
      .next

  val start1: Long = System.currentTimeMillis
  val answer1: Int = answer(numberOfArgs = 2, sumEquals = 2020)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = answer(numberOfArgs = 3, sumEquals = 2020)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
