import scala.io._

object Day01 {

  val input: Set[Int] =
    Source
      .fromFile("src/resources/input01.txt")
      .getLines
      .map(_.toInt)
      .toSet

  val solutionsPart1: Set[Set[Int]] =
    for {
      a <- input
      b <- input
      if (a + b == 2020)
    } yield Set(a, b)

  val solutionsPart2: Set[Set[Int]] =
    for {
      a <- input
      b <- input
      c <- input
      if (a + b + c == 2020)
    } yield Set(a, b, c)
  
  def main(args: Array[String]): Unit = {
    println(s"Answers part 1: ${solutionsPart1.map(_.foldLeft(1)(_*_))}")
    println(s"Answers part 2: ${solutionsPart2.map(_.foldLeft(1)(_*_))}")
  }
}
