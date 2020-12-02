import scala.io._

object Day02 {

  type Pred = Int => Int => Char => String => Boolean

  val Line = "(\\d+)-(\\d+)\\s(.):\\s(.+)".r

  def answer(pred: Pred): Int =
    Source
      .fromFile("src/resources/input02.txt")
      .getLines
      .filter {
        case Line(min,max,char,passwd) =>
          pred(min.toInt)(max.toInt)(char.head)(passwd) 
      }
      .toList
      .size

  def check1: Pred =
    min => max => char => passwd => {
      val count = passwd.filter(_ == char).size
      (count >= min && count <= max)
    }
    
  def check2: Pred =
    idx0 => idx1 => char => passwd => {
      val chars = passwd.zipWithIndex.map((c,i) => (i,c)).toMap
      chars.get(idx0 - 1) == Some(char) ^ chars.get(idx1 - 1) == Some(char)
    }
    
  def main(args: Array[String]): Unit = {
    println(s"Answer part 1: ${answer(check1)}")
    println(s"Answer part 2: ${answer(check2)}")
  }
}
