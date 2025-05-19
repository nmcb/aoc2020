import scala.io.*
import scala.annotation.tailrec

object Day03 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Forest(tile: Vector[Vector[Char]]):
    val sizeX: Int = tile(0).size
    val sizeY: Int = tile.size

    def sample(x: Int, y: Int): Option[Char] =
      Option.when(y < sizeY)(tile(y)(x % sizeX))

    def walk(dx: Int, dy: Int): Long =
      @tailrec
      def step(x: Int, y: Int, acc: Long = 0): Long =
        sample(x, y) match
          case None      => acc
          case Some('.') => step(x + dx, y + dy, acc)
          case Some('#') => step(x + dx, y + dy, acc + 1)
          case _         => sys.error(s"boom: x=$x,y=$y,dx=$dx,dy=$dy")
      step(dx,dy)

  val forest: Forest =
    Forest(
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .map(_.toVector)
        .toVector
    )

  val start1  = System.currentTimeMillis
  val answer1 = forest.walk(3, 1)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val walks: List[Long] =
    List(
      forest.walk(1,1),
      forest.walk(3,1),
      forest.walk(5,1),
      forest.walk(7,1),
      forest.walk(1,2)
    )

  val start2  = System.currentTimeMillis
  val answer2 = walks.product
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
