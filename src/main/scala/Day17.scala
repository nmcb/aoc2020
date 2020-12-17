import scala.io._
import scala.collection._

object Day17 extends App {

  type Cube  = (Int,Int,Int,Int)
  type Space = List[Cube]

  case class Pocket(space: Space) {

    def maxX: Int = space.map(_._1).max
    def minX: Int = space.map(_._1).min
    def maxY: Int = space.map(_._2).max
    def minY: Int = space.map(_._2).min
    def maxZ: Int = space.map(_._3).max
    def minZ: Int = space.map(_._3).min
    def maxW: Int = space.map(_._4).max
    def minW: Int = space.map(_._4).min

    def active(x: Int, y: Int, z: Int, w: Int): Boolean =
      space.exists(_ == (x,y,z,w))

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
      val lb = mutable.ListBuffer.empty[Cube]
      for (x <- minX-1 to maxX+1) {
        for (y <- minY-1 to maxY+1) {
          for (z <- minZ-1 to maxZ+1) {
            for (w <- minW-1 to maxW+1) {
              val neighbours = countNeighbours(x,y,z,w)
              if (inactive(x,y,z,w) && (neighbours == 3))
                lb += ((x,y,z,w))
              else if (active(x,y,z,w) && (neighbours == 2 || neighbours == 3))
                lb += ((x,y,z,w))
      }}}}
      val l = lb.toList
      println(s"l.size=${l.size}")
      Pocket(lb.toList)
    }
  }

  val pocket =
    Pocket({
      Source
        .fromFile("src/resources/input17.txt")
        .getLines
        .zipWithIndex
        .flatMap { 
          case (line,y) => line.zipWithIndex.map {
            case (c,x) => (x,y,0,0) -> c
          }
        }.toList
        .filter {
          case (_,c) => c == '#'
        }
        .map(_._1)
    })

  val start1 = System.currentTimeMillis

  val p1 = pocket.next
  val p2 = p1.next
  val p3 = p2.next
  val p4 = p3.next
  val p5 = p4.next
  val p6 = p5.next

  // part 2 was hacked by just adding the w-dimension, the solution was fast enough...
  println(s"Answer part 2: ${p6.space.size} [${System.currentTimeMillis - start1}ms]")
}
