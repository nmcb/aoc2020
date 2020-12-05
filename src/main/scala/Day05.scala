import scala.io._

object Day05 {

  val input =
      Source
        .fromFile("src/resources/input05.txt")
        .getLines
        .toList

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

  val answer1 =
    input.map(seatId).sorted.reverse

  val answer2 = {
    val ids = input.map(s => seatId(s) -> s).toMap
    input.foldLeft(Option.empty[Int])((a,s) => (a, seatId(s)) match {
      case (None, test) =>
        if (!ids.keySet.contains(test + 1) && ids.keySet.contains(test + 2))
          Some(test + 1)
        else
          None
      case (solution, _) =>
        solution
    })
  }

  def main(args: Array[String]): Unit = {

    println(s"Answer part 1: ${answer1.headOption}")
    println(s"Answer part 2: ${answer2}")
  }
}
