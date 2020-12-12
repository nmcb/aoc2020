import scala.io._
import scala.annotation.tailrec
import scala.util.control.Breaks._
import scala.collection._

object Day12 extends App {

  case class Action(override val toString: String) {
    def code: Char = toString.head
    def by: Int    = toString.tail.toInt
  }

  object Action {
    def fromString(str: String): Action = Action(str)
  }

  case class Ship(x: Int = 0, y: Int = 0, face: Char = 'E') {

    @tailrec private final def reface(f: Char, c: Char, by: Int): Char =
      if (by == 0) f else (f,c) match {
        case ('N','R') => reface('E', c, by - 90)
        case ('E','R') => reface('S', c, by - 90)
        case ('S','R') => reface('W', c, by - 90)
        case ('W','R') => reface('N', c, by - 90)
        case ('N','L') => reface('W', c, by - 90)
        case ('E','L') => reface('N', c, by - 90)
        case ('S','L') => reface('E', c, by - 90)
        case ('W','L') => reface('S', c, by - 90)
        case _         => sys.error(s"two faced ${(f,c)}")
      }


    def perform(a: Action): Ship = {
      a.code match {
        case 'N'       => copy(y = y + a.by)
        case 'S'       => copy(y = y - a.by)
        case 'E'       => copy(x = x + a.by)
        case 'W'       => copy(x = x - a.by)
        case 'L' | 'R' => copy(face = reface(face, a.code, a.by))
        case 'F'       => face match {
                            case 'N' => copy(y = y + a.by)
                            case 'S' => copy(y = y - a.by)
                            case 'E' => copy(x = x + a.by)
                            case 'W' => copy(x = x - a.by)
                            case _   => sys.error(s"flat on it's face=$face")
                          }
        case _   => sys.error("code contains a coding error")
      }
    }

    def distance: Int =
      x.abs + y.abs
  }

  val start1 = System.currentTimeMillis

  val answer1: Int =
      Source
        .fromFile("src/resources/input12.txt")
        .getLines
        .map(Action.fromString)
        .toList
        .map(a => {println(a);a})
        .foldLeft(Ship())((s,a) => s.perform(a))
        .distance

  println(s"Answer part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  case class WayPoint(x: Int, y: Int) {

    @tailrec private final def rotate(c: Char, by: Int, x0: Int = x, y0: Int = y): WayPoint = 
      if (by == 0) then copy(x = x0, y = y0) else c match {
        case 'R' => rotate(c, by - 90, y0, 0 - x0)
        case 'L' => rotate(c, by - 90, 0 - y0, x0)
        case _   => sys.error("code contains a coding error")
      }

    def perform(a: Action): WayPoint =
        a.code match {
          case 'N' => copy(y = y + a.by)
          case 'S' => copy(y = y - a.by)
          case 'E' => copy(x = x + a.by)
          case 'W' => copy(x = x - a.by)
          case deg => rotate(deg, a.by)
        }
  }

  case class WayPointedShip(x: Int = 0, y: Int = 0, wp: WayPoint = WayPoint(10, 1)) {

    def perform(a: Action): WayPointedShip = {
      a.code match {
        case 'N' | 'S' | 'E' | 'W' | 'L' | 'R' =>
          copy(wp = wp.perform(a))
        case 'F' =>
          copy(x = (wp.x * a.by) + x, y = (wp.y * a.by) + y)
        case _   =>
          sys.error("code contains a coding error")
      }
    }

    def distance: Int =
      x.abs + y.abs
  }

  val start2 = System.currentTimeMillis

  val answer2: Int =
      Source
        .fromFile("src/resources/input12.txt")
        .getLines
        .map(Action.fromString)
        .toList
        .map(a => {println(a);a})
        .foldLeft(WayPointedShip())((s,a) => s.perform(a))
        .distance

  println(s"Answer part 2: ${answer2} [${System.currentTimeMillis - start1}ms]")

}