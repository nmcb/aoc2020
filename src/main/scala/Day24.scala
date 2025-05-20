import scala.annotation.tailrec
import scala.io.Source

object Day24 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Tile(x: Int, y: Int):
    def even: Boolean = y % 2 == 0
    def east: Tile = Tile(x + 1, y)
    def west: Tile = Tile(x - 1, y)
    def ne: Tile = if even then Tile(x + 1, y + 1) else Tile(x    , y + 1)
    def nw: Tile = if even then Tile(x    , y + 1) else Tile(x - 1, y + 1)
    def se: Tile = if even then Tile(x + 1, y - 1) else Tile(x    , y - 1)
    def sw: Tile = if even then Tile(x    , y - 1) else Tile(x - 1, y - 1)
    def neighbours: Vector[Tile] = Vector(this, east, west, ne, nw, se, sw)

  type Floor = Set[Tile]

  extension (floor: Floor)
    def adjacent(tile: Tile): Int = tile.neighbours.count(floor.contains)

  def solve1(flips: Vector[String]): Floor =
    flips
      .map: directions =>
        @tailrec
        def go(point: Tile, remaining: String): Tile =
          if remaining.isEmpty then
            point
          else
            remaining.head match
              case 'e' => go(point.east, remaining.tail)
              case 'w' => go(point.west, remaining.tail)
              case 'n' => remaining(1) match
                case 'e' => go(point.ne, remaining.drop(2))
                case 'w' => go(point.nw, remaining.drop(2))
              case 's' => remaining(1) match
                case 'e' => go(point.se, remaining.drop(2))
                case 'w' => go(point.sw, remaining.drop(2))
        go(Tile(0, 0), directions)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .filter(_._2 % 2 == 1)
      .keySet

  val flips = Source.fromResource(s"input$day.txt").getLines().toVector

  val start1 = System.currentTimeMillis
  val answer1 = solve1(flips).size
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def solve2(flips: Vector[String]): Int =
    def step(floor: Set[Tile]): Set[Tile] =
      floor
        .flatMap(_.neighbours)
        .flatMap: tile =>
          (floor.contains(tile), floor.adjacent(tile)) match
            case (_, 2) | (true, 3) => Some(tile)
            case _                  => None

    Iterator.iterate(solve1(flips))(step).drop(100).next.size

  val start2  = System.currentTimeMillis
  val answer2 = solve2(flips)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
