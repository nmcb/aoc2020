import scala.io._

object Day01 {

  val input: List[Int] =
    Source
      .fromFile("src/resources/input01.txt")
      .getLines
      .map(_.toInt)
      .toList

  val solutionsPart1: List[(Int,Int)] =
    for {
      a <- input
      b <- input
      if (a + b == 2020)
    } yield (a, b)

  val solutionsPart2: List[(Int,Int,Int)] =
    for {
      a <- input
      b <- input
      c <- input
      if (a + b + c == 2020)
    } yield (a, b, c)
  
  def main(args: Array[String]): Unit = {
    println(s"Answers part 1: ${solutionsPart1.map(s => s._1 * s._2)}")
    println(s"Answers part 2: ${solutionsPart2.map(s => s._1 * s._2 * s._3)}")
  }
}
