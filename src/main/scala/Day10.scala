import scala.io._
import scala.annotation.tailrec
import scala.util.control.Breaks._
import scala.collection._

object Day10 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Adapter(rated: Int):

    def inputs: List[Int] =
      if      rated == 1 then List(0)
      else if rated == 2 then List(0,1)
      else                    List(rated - 1, rated - 2, rated - 3)

    def output: Int           = rated
    def diff(input: Int): Int = rated - input


  val ratings: List[Int] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.toInt)
      .toList
      .sorted

  def solve1(ratings: List[Int]): Int =
    val (output, diff1, diff3) = 
      ratings
        .foldLeft((0,0,1)):
          case ((input,diff1,diff3),rated) =>
            val adapter = Adapter(rated)
            val (left, right) = if adapter.diff(input) == 1 then (diff1 + 1, diff3) else (diff1, diff3 + 1)
            (adapter.output, left, right)

    diff1 * diff3

  val start1  = System.currentTimeMillis
  val answer1 = solve1(ratings)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  
  val sorted = ratings.sorted
  val search = (0 :: sorted) :+ (sorted.max + 3) // [0] :: as :: [max(as) + 3]
  val paths  = mutable.Map(0 -> 1L)              // V is # of paths from rating [0] to rating K
  for i <- 1 until search.length do              // loop rating i from search(1) to rs(search.length-1)
    breakable {
      for j <- (0 to i).reverse do               // loop rating j from rating(i) back to rating(0)

    // if diff ratings > 3 then break on j else upsert # paths of rating i adding # paths of j
    if search(i) - search(j) > 3 then
      break
    else
      paths += i -> (paths.getOrElse(i, 0L) + paths.getOrElse(j, 0L))
  }

  // # of rating configurations is # paths to last rating
  val start2 = System.currentTimeMillis
  val answer2: Long = paths(search.length - 1)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
