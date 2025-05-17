import scala.io._

object Day02 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  type Pred = Int => Int => Char => String => Boolean

  val Line = "(\\d+)-(\\d+)\\s(.):\\s(.+)".r

  def answer(pred: Pred): Int =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .count:
        case s"$x-$y $char: $password" => pred(x.toInt)(y.toInt)(char.head)(password)

  def check1: Pred = min => max => char => passwd =>
    val count = passwd.count(_ == char)
    count >= min && count <= max
    
  def check2: Pred = x => y => char => passwd =>
    val chars = passwd.zipWithIndex.map(_.swap).toMap
    chars.get(x - 1).contains(char) ^ chars.get(y - 1).contains(char)


  val start1  = System.currentTimeMillis
  val answer1 = answer(check1)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = answer(check2)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
