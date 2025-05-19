import scala.collection.*

object Day15 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val numbers: List[Int] = "5,1,9,18,13,8,0".split(',').map(_.toInt).toList

  def solve(numbers: List[Int], turns: Int): Int =
    val map = numbers.dropRight(1).zipWithIndex.toMap
    val (_, result) = (numbers.length until turns).foldLeft((map,numbers.last)):
      case ((map,last),index) if map.contains(last) => (map.updated(last, index - 1), index - 1 - map(last))
      case ((map,last),index)                       => (map.updated(last, index - 1), 0)
    result

  val start1  = System.currentTimeMillis
  val answer1 = solve(numbers, 2020)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = solve(numbers, 30000000)
  println(s"Answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
  