import scala.io._
import scala.collection._

object Day17 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  type Space = Map[Int,Map[Int,Map[Int,Set[Int]]]]

  case class Pocket(space: Space) {

    def maxX: Int = space.keys.max
    def minX: Int = space.keys.min
    def maxY: Int = space(0).keys.max
    def minY: Int = space(0).keys.min
    def maxZ: Int = space(0)(0).keys.max
    def minZ: Int = space(0)(0).keys.min
    def maxW: Int = space(0)(0)(0).max
    def minW: Int = space(0)(0)(0).min

    def active(x: Int, y: Int, z: Int, w: Int): Boolean =
      space(x)(y)(z).contains(w)

    def inactive(x: Int, y: Int, z: Int, w: Int): Boolean =
      !active(x,y,z,w)

    def countNeighbours(x: Int, y: Int, z: Int, w: Int): Int = {
      var count = 0
      for (xi <- x-1 to x+1) {
        for (yi <- y-1 to y+1) {
          for (zi <- z-1 to z+1) {
            for (wi <- w-1 to w+1) {
              if (active(xi,yi,zi,wi) && !(xi==x && yi==y && zi==z && wi==w)) count += 1
      }}}}
      count
    }

    def next: Pocket = {
      var xs = Map[Int,Map[Int,Map[Int,Set[Int]]]]()
      for (x <- minX-1 to maxX+1) {
        var ys = Map[Int,Map[Int,Set[Int]]]()
        for (y <- minY-1 to maxY+1) {
          var zs = Map[Int,Set[Int]]()
          for (z <- minZ-1 to maxZ+1) {
            var ws = Set[Int]()
            for (w <- minW-1 to maxW+1) {
              val neighbours = countNeighbours(x,y,z,w)
              if (inactive(x,y,z,w) && (neighbours == 3)) {
                ws = ws ++ Set(w)
                zs = zs ++ Map(z -> ws)
                ys = ys ++ Map(y -> zs)
                xs = xs ++ Map(x -> ys)
              }
              else if (active(x,y,z,w) && (neighbours == 2 || neighbours == 3)) {
                ws = ws ++ Set(w)
                zs = zs ++ Map(z -> ws)
                ys = ys ++ Map(y -> zs)
                xs = xs ++ Map(x -> ys)
              }
      }}}}
      Pocket(xs)
    }

    def size: Int =
      space.foldLeft(0) {
        case (a1,(_,ys)) =>
          a1 + ys.foldLeft(0) {
            case (a2,(_,zs)) =>
              a2 + ys.foldLeft(0) {
                case (a3,(_,ws)) =>
                  a3 + ws.size
              }
          }
      }
  }

  val pocket = {
    val input =
      Source
        .fromFile("src/resources/input17.txt")
        .getLines
        .zipWithIndex
        .foldLeft(Map[Int,Map[Int,Map[Int,Set[Int]]]]()) {
          case (xs,(ln,x)) =>
            xs ++ Map(x -> ln.zipWithIndex
              .foldLeft(Map[Int,Map[Int,Set[Int]]]()) {
                case (ys,(c,y)) =>
                  if (c == '#') then {
                    ys ++ Map(y -> Map(0 -> Set(0)))
                  } else {
                    ys
                  }
              }
            )
        }

    Pocket(input)
  }

  val start1 = System.currentTimeMillis

  val p1 = pocket.next
  println(s"1 [${System.currentTimeMillis - start1}ms]")
  val p2 = p1.next
  println(s"2 [${System.currentTimeMillis - start1}ms]")
  val p3 = p2.next
  println(s"3 [${System.currentTimeMillis - start1}ms]")
  val p4 = p3.next
  println(s"4 [${System.currentTimeMillis - start1}ms]")
  val p5 = p4.next
  println(s"5 [${System.currentTimeMillis - start1}ms]")
  val p6 = p5.next
  println(s"6 [${System.currentTimeMillis - start1}ms]")

  // part 2 was hacked by just adding the w-dimension, the solution was fast enough...
  println(s"Answer part 2: ${p6.size} [${System.currentTimeMillis - start1}ms]")
