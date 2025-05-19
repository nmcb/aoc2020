import scala.annotation.tailrec
import scala.io.Source

object Day20 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val monster =
    """                  #
      |#    ##    ##    ###
      | #  #  #  #  #  #
      |"""
      .stripMargin
      .split("\n")
      .toSeq
      .zipWithIndex
      .flatMap: (line,y) =>
        line
          .zipWithIndex
          .collect:
            case (c,x)if c == '#' => (x,y)

  case class Pos(x: Int, y: Int):
    infix def plus(dx: Int, dy: Int): Pos = Pos(x + dx, y + dy)

  /** note that images are squares */
  case class Image(size: Int, pixels: Map[Pos, Char]):
    val top    = (0 until size).map(x => pixels(Pos(x, 0)))
    val left   = (0 until size).map(y => pixels(Pos(0, y)))
    val bottom = (0 until size).map(x => pixels(Pos(x, size - 1)))
    val right  = (0 until size).map(y => pixels(Pos(size - 1, y)))

    private def rotateCW: Image =
      Image(size, pixels.map((pos, pixel) => Pos(size - pos.y - 1, pos.x) -> pixel))

    private def flipH: Image =
      Image(size, pixels.map((pos, pixel) => Pos(size - pos.x - 1, pos.y) -> pixel))

    private def flipV: Image =
      Image(size, pixels.map((pos, pixel) => Pos(pos.x, size - pos.y - 1) -> pixel))

    def permutations: Vector[Image] =
      import Iterator.*
      val all =
        for
          rotate <- 0 to 3
          fliph  <- 0 to 1
          flipv  <- 0 to 1
        yield
          val rotated = iterate(this)(_.rotateCW).drop(rotate).next
          val flipped = iterate(rotated)(_.flipH).drop(fliph).next
          iterate(flipped)(_.flipV).drop(flipv).next
      all.distinct.toVector

  case class Tile(id: Long, image: Image):
    export image.{top, left, bottom, right}

    val edges =
      val forward   = Set(top, left, bottom, right)
      val backwards = forward.map(_.reverse)
      forward ++ backwards

    def adjacentTo(other: Tile): Boolean =
      other.edges.intersect(edges).nonEmpty

    def permutations: Seq[Tile] =
      image.permutations.map(Tile(id, _))

  def corners(tiles: Vector[Tile]): Vector[Tile] =
    val occurrences = tiles.flatMap(_.edges).groupMapReduce(identity)(_ => 1)(_ + _)
    val categories  = tiles.groupBy(tile => tile.edges.count(edge => occurrences(edge) == 1))
    categories(4)

  val tiles: Vector[Tile] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim.split("\n\n").toSeq
      .map(_.split("\n").map(_.trim))
      .map: tile =>
        val id     = tile.head.slice(5, 9).toLong
        val data   = tile.drop(1)
        val pixels = for y <- 0 to 9; x <- 0 to 9 yield Pos(x, y) -> data(y)(x)
        Tile(id, Image(10, pixels.toMap))
      .toVector

  def solve1(tiles: Vector[Tile]): Long =
    corners(tiles).map(_.id).product

  val start1 = System.currentTimeMillis
  val answer1 = solve1(tiles)
  println(s"Answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")
  assert(answer1 == 20913499394191L)

  def unscrambleTiles(tiles: Vector[Tile]): Map[Pos, Tile] =
    val size = math.sqrt(tiles.size).toInt
    val positions = for x <- 0 until size; y <- 0 until size yield Pos(x, y)
    val corner = corners(tiles).head.permutations
    val occurrences = tiles.flatMap(_.edges).groupMapReduce(identity)(_ => 1)(_ + _)

    @tailrec
    def go(remaining: Vector[Tile], positions: Vector[Pos], ordered: Map[Pos, Tile]): Map[Pos, Tile] =
      if positions.isEmpty then
        ordered
      else
        val tile = positions.head match
          case Pos(0, 0) => corner.filter(tile => occurrences(tile.top) == 1 && occurrences(tile.left) == 1).head
          case Pos(0, y) => remaining.filter(tile => tile.top == ordered(Pos(0, y - 1)).bottom).head
          case Pos(x, y) => remaining.filter(tile => tile.left == ordered(Pos(x - 1, y)).right).head
        go(remaining.filterNot(_.id == tile.id), positions.tail, ordered.updated(positions.head, tile))

    go(tiles.flatMap(_.permutations), positions.toVector, Map.empty)

  def assembleImage(unscrambled: Map[Pos, Tile]): Image =
    val size = 8 * math.sqrt(unscrambled.size).toInt
    val pixels = for
      x <- 0 until size
      y <- 0 until size
    yield
      Pos(x, y) -> unscrambled(Pos(x / 8, y / 8)).image.pixels(Pos(1 + x % 8, 1 + y % 8))

    Image(size, pixels.toMap)

  def findMonsters(image: Image): Vector[Int] =
    for
      candidate <- image.permutations
    yield
      val matches = for
        x <- 0 until image.size - 20 // monster dimensions are 20 x 3
        y <- 0 until image.size - 3
        if monster.map(Pos(x, y).plus).forall(pos => candidate.pixels(pos) == '#')
      yield (x, y)
      val monsters = matches.map(Pos(_, _)).flatMap(pos => monster.map(pos.plus)).toSet
      candidate.pixels.keys.count(pos => candidate.pixels(pos) == '#' && !monsters.contains(pos))

  def solve2(tiles: Vector[Tile]): Long =
    val unscrambled   = unscrambleTiles(tiles)
    val completeImage = assembleImage(unscrambled)
    val roughness     = findMonsters(completeImage)
    roughness.min

  val start2 = System.currentTimeMillis
  val answer2 = solve2(tiles)
  println(s"Answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
  assert(answer2 == 2209)

