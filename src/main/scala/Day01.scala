import scala.io._

object Day01 {

  val input: Set[Int] =
    Source
      .fromFile("src/resources/input01.txt")
      .getLines
      .map(_.toInt)
      .toSet

  val distinct3: Set[Set[Int]] =
    for { a <- input ; b <- input ; c <- input ; } yield Set(a, b, c)

  val mul: Set[Int] => Int =
    _.foldLeft(1)(_*_)
  
  val sum: Set[Int] => Int =
    _.foldLeft(0)(_+_)

  def byNumberOfArgs(n: Int): Set[Int] => Boolean =
    _.size == n

  def bySumEquals(n: Int): Set[Int] => Boolean =
    sum(_) == n

  def answer(numberOfArgs: Int)(sumEquals: Int): Option[Int] =
    distinct3
      .filter(byNumberOfArgs(numberOfArgs))
      .filter(bySumEquals(sumEquals))
      .map(mul)
      .headOption
    
  def main(args: Array[String]): Unit = {
    println(s"Answer part 1: ${answer(2)(2020)}")
    println(s"Answer part 2: ${answer(3)(2020)}")
  }
}
