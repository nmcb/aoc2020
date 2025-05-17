import scala.io._
import scala.annotation.tailrec
import scala.util.control.Breaks._
import scala.collection._

object Day10 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

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

  val as = Source.fromFile("src/resources/input10.txt").getLines.map(_.toInt).toList.sorted
  val rs = (0 :: as) :+ (as.max + 3) // [0] :: as :: [max(as) + 3]
  val ps = mutable.Map(0 -> 1L)      // V is # of paths from rating [0] to rating K
  
  for(i <- 1 until rs.length) {              // loop rating i from rs(1) to rs(rs.length-1)
    breakable { for(j <- (0 to i).reverse) { // loop rating j from rs(i) back to rs(0)

    // if diff ratings > 3 then break j else upsert # paths of rating i adding # paths of j
    if (rs(i) - rs(j) > 3) break else ps += i -> (ps.getOrElse(i, 0L) + ps.getOrElse(j, 0L))
  }}}

  // # of rating configurations is # paths to last rating
  println(s"Answer part 2: ${ps(rs.length - 1)} [${System.currentTimeMillis - start2}ms]")
