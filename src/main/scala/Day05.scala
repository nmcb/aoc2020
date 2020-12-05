import scala.io._

object Day05 {

  def seatId(s: String): Int = {
    import Integer._
    val row = parseInt(s.substring(0,7).replaceAll("F","0").replaceAll("B","1"), 2)
    val col = parseInt(s.substring(7,10).replaceAll("L","0").replaceAll("R","1"), 2)
    row * 8 + col
  }

  assert(seatId("FBFBBFFRLR") == 357)
  assert(seatId("BFFFBBFRRR") == 567)
  assert(seatId("FFFBBBFRRR") == 119)
  assert(seatId("BBFFBBFRLL") == 820)

  val input: Seq[Int] =
    Source
      .fromFile("src/resources/input05.txt")
      .getLines
      .map(s => seatId(s))
      .toSeq

  val answer1: Option[Int] =
    input.sorted.reverse.headOption

  val answer2: Option[Int] = {
    input.foldLeft(Option.empty[Int])((a,id) => (a, id) match {
      case (None, test) =>
        if (!input.contains(test + 1) && input.contains(test + 2))
          Some(test + 1)
        else
          None
      case (solution, _) =>
        solution
    })
  }

  def main(args: Array[String]): Unit = {

    println(s"Answer part 1: ${answer1}")
    println(s"Answer part 2: ${answer2}")
  }
}
