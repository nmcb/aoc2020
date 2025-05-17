import scala.io._

object Day02 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  type Pred = Int => Int => Char => String => Boolean

  val Line = "(\\d+)-(\\d+)\\s(.):\\s(.+)".r

  def answer(pred: Pred): Int =
    Source
      .fromFile("src/resources/input02.txt")
      .getLines
      .filter {
        case Line(int1, int2, char, passwd) =>
          pred(int1.toInt)(int2.toInt)(char.head)(passwd) 
      }
      .toList
      .size

  def check1: Pred =
    min => max => char => passwd => {
      val count = passwd.filter(_ == char).size
      count >= min && count <= max
    }
    
  def check2: Pred =
    pos1 => pos2 => char => passwd => {
      val chars = passwd.zipWithIndex.map(_.swap).toMap
      chars.get(pos1 - 1) == Some(char) ^ chars.get(pos2 - 1) == Some(char)
    }
    
  println(s"Answer part 1: ${answer(check1)}")
  println(s"Answer part 2: ${answer(check2)}")
