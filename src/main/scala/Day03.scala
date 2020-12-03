import scala.io._

object Day03 {

  case class Forest(tile: List[List[Char]]) {
    assert(tile.size >= 1, "tile is empty on the y-axis")
    assert(tile.map(_.size).distinct.size == 1, "tile is not rectangular")
    assert(tile.map(_.size).distinct.headOption.filter(_ >= 1).nonEmpty, "tile is empty on the x-axis")
    assert(tile.flatten.filter(c => c != '.' && c != '#').size == 0, "tile invalid")

    val width: Long =
      tile(0).size

    val height: Long =
      tile.size

    def sample(x: Long, y: Long): Option[Char] = {
      if (y >= height)
        None
      else
        Some(tile(y.toInt)((x % width).toInt))
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

  val forest: Forest =
    Forest(
      Source
        .fromFile("src/resources/input03.txt")
        .getLines
        .map(_.toList)
        .toList)

  def main(args: Array[String]): Unit = {

    println(s"Answer part 1: ${forest.walk(3, 1)}")

    val walks: List[Long] =
      List( forest.walk(1, 1)
          , forest.walk(3, 1)
          , forest.walk(5, 1)
          , forest.walk(7, 1)
          , forest.walk(1, 2)
          )

    println(s"Answer part 2: ${walks.product}")
  }
}