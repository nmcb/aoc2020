import scala.io._
import scala.annotation.tailrec
import scala.util.control.Breaks._
import scala.collection._

object Day10 extends App {

  case class Adapter(rated: Int) {
    assert(rated > 0, s"non-positive adater of $rated")
    def inputs: List[Int] =
      if      (rated == 1) List(0)
      else if (rated == 2) List(0,1)
      else                 List(rated - 1, rated - 2, rated - 3)
    def output: Int =
      rated
    def diff(input: Int): Int =
      rated - input
  }

  val adapters: List[Int] =
    Source
      .fromFile("src/resources/input10.txt")
      .getLines
      .map(_.toInt)
      .toList
      .sorted

  val start1 = System.currentTimeMillis

  val answer1: Int = {
    val (output, diff1, diff3) = 
      adapters
        .foldLeft((0,0,1)){
          case ((input,diff1,diff3),rated) =>
            val adater = Adapter(rated)
            val diff = if (adater.diff(input) == 1) (diff1 + 1, diff3) else (diff1, diff3 + 1)
            (adater.output, diff._1, diff._2)
        }
    diff1 * diff3
  }

  println(s"Answer part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2 = System.currentTimeMillis

  val input   = Source.fromFile("src/resources/input10.txt").getLines.map(_.toInt).toList.sorted
  val ratings = (0 :: input) :+ (input.max + 3)
  val paths   = mutable.Map(0 -> 1L)
  
  for(i <- 1 until ratings.size) {
    breakable {
      for(j <- (0 to i).reverse) {
        if (ratings(i) - ratings(j) > 3)
          break
        else
          paths += i -> (paths.getOrElse(i, 0L) + paths.getOrElse(j, 0L))
      }
    }
  }

  println(s"Answer part 2: ${paths(ratings.size - 1)} [${System.currentTimeMillis - start2}ms]")
}
