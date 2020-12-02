import scala.io._

object Day01 {

  val input: List[Int] =
    Source
      .fromFile("src/resources/input01.txt")
      .getLines
      .map(_.toInt)
      .toList

  def answer(numberOfArgs: Int)(sumEquals: Int): Option[Int] =
    input
      .combinations(numberOfArgs)
      .filter(_.sum == sumEquals)
      .map(_.product)
      .toList
      .headOption
    
  def main(args: Array[String]): Unit = {
    println(s"Answer part 1: ${answer(2)(2020)}")
    println(s"Answer part 2: ${answer(3)(2020)}")
  }
}
