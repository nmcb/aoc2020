import scala.io._

object Day03 {

  case class Rect(cont: List[List[Char]]) {

    val width: Long =
      cont(0).size

    val height: Long =
      cont.size

    def sample(x: Long, y: Long): Option[Char] = {
      if (y >= height)
        None
      else
        Some(cont(y.toInt)((x % width).toInt))
    }

    def walk(dx: Long, dy: Long): Long = {
      def step(x: Long, y: Long, acc: Long = 0): Long = {
        sample(x, y) match {
          case None      => acc
          case Some('.') => step(x + dx, y + dy, acc)
          case Some('#') => step(x + dx, y + dy, acc + 1)
          case _         => sys.error(s"boom: x=$x,y=$y,dx=$dx,dy=$dy")
        }
      }
      step(dx, dy)
    }
  }

  val rect: Rect =
    Rect(
      Source
        .fromFile("src/resources/input03.txt")
        .getLines
        .map(_.toList)
        .toList)

  def main(args: Array[String]): Unit = {
    val walk_1_1 = rect.walk(1, 1)
    val walk_3_1 = rect.walk(3, 1)
    val walk_5_1 = rect.walk(5, 1)
    val walk_7_1 = rect.walk(7, 1)
    val walk_1_2 = rect.walk(1, 2)
    println(s"Answer part 1: ${walk_3_1}")
    println(s"Answer part 2: ${walk_1_1 * walk_3_1 * walk_5_1 * walk_7_1 * walk_1_2}")
  }
}
