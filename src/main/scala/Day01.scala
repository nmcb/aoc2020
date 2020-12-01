import scala.io._

object Day01 {

  val input: Set[Int] =
    Source
      .fromFile("src/resources/input01.txt")
      .getLines
      .map(_.toInt)
      .toSet

  val tabulated3: Set[Set[Int]] =
    for { a <- input ; b <- input ; c <- input ; } yield Set(a, b, c)

  val mul: Set[Int] => Int =
    _.foldLeft(1)(_*_)
  
  val sum: Set[Int] => Int =
    _.foldLeft(0)(_+_)

  def byNumberOfArgs(n: Int): Set[Int] => Boolean =
    _.size == n

  def bySumEquals(n: Int): Set[Int] => Boolean =
    sum(_) == n

  def answerByNumberOfArgs(n: Int): Option[Int] =
    tabulated3
      .filter(byNumberOfArgs(n))
      .filter(bySumEquals(2020))
      .map(mul)
      .headOption
    
  def main(args: Array[String]): Unit = {
    println(s"Answer part 1: ${answerByNumberOfArgs(2)}")
    println(s"Answer part 2: ${answerByNumberOfArgs(3)}")
  }
}
