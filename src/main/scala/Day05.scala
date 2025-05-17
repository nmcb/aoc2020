import scala.io._

object Day05:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  import Integer._

  def seatId(s: String): Int = {
    val row = parseInt(s.substring(0, 7).replaceAll("F","0").replaceAll("B","1"), 2)
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
      .map(seatId)
      .toSeq

  val answer1: Option[Int] =
    input.sorted.lastOption

  val answer2: Option[Int] =
    input.foldLeft(Option.empty[Int]) {
      case (None, id) if !input.contains(id + 1) && input.contains(id + 2) => Some(id + 1)
      case (acc, _)                                                        => acc
    }

  println(s"Answer part 1: ${answer1}")
  println(s"Answer part 2: ${answer2}")
