import scala.io._

object Day06 {

  val input: List[String] =
    Source
      .fromFile("src/resources/input06.txt")
      .getLines
      .toList

  val answers: List[List[String]] =
    input.foldLeft(List(List.empty[String])) {
      case (acc, line) if line.nonEmpty => (line :: acc.head) :: acc.tail
      case (acc, _) => List.empty[String] :: acc
    }
    
  val answer1: List[Int] =
    answers
      .map(_.foldLeft("")(_+_).toSet)
      .map(_.size)

  val answer2: List[Int] =
    answers.map(grp => {
      val all = grp.foldLeft("")(_+_).toSet
      val dis = grp.map(_.toSet).foldLeft(all)(_ intersect _)
      dis.size
    })

  def main(args: Array[String]): Unit = {
    println(s"Answer part 1: ${answer1.sum}")
    println(s"Answer part 2: ${answer2.sum}")
  }
}
