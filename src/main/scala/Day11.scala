import scala.io._
import scala.annotation.tailrec
import scala.util.control.Breaks._
import scala.collection._

object Day11 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Floor(floor: List[String]) {

    def seat(ix: Int, iy: Int): Option[Char] =
      if (ix < 0 || ix >= floor.head.length || iy < 0 || iy >= floor.length) None
      else Some(floor(iy)(ix)).filter(c => c == 'L' || c == '#')

    def count1(ix: Int, iy: Int): Int = {
      val N  = seat(ix    , iy - 1).count(_ == '#')
      val E  = seat(ix + 1, iy    ).count(_ == '#')
      val S  = seat(ix    , iy + 1).count(_ == '#')
      val W  = seat(ix - 1, iy    ).count(_ == '#')
      val NE = seat(ix + 1, iy - 1).count(_ == '#')
      val NW = seat(ix - 1, iy - 1).count(_ == '#')
      val SE = seat(ix + 1, iy + 1).count(_ == '#')
      val SW = seat(ix - 1, iy + 1).count(_ == '#')
      List(NW,N,NE,E,SE,S,SW,W).sum
    }

    def count2(ix: Int, iy: Int): Int = {
      def find(x: Int, y: Int, dx: Int, dy: Int): Option[Char] =
        if (x < 0 || x >= floor.head.length || y < 0 || y >= floor.length)
          None
        else
          seat(x + dx, y + dy) match {
            case None          => find(x + dx, y + dy, dx, dy)
            case s: Some[Char] => s
          }
      
      val N  = find(ix, iy,  0, -1).count(_ == '#')
      val E  = find(ix, iy, +1,  0).count(_ == '#')
      val S  = find(ix, iy,  0, +1).count(_ == '#')
      val W  = find(ix, iy, -1,  0).count(_ == '#')
      val NE = find(ix, iy, +1, -1).count(_ == '#')
      val NW = find(ix, iy, -1, -1).count(_ == '#')
      val SE = find(ix, iy, +1, +1).count(_ == '#')
      val SW = find(ix, iy, -1, +1).count(_ == '#')
      List(NW,N,NE,E,SE,S,SW,W).sum
    }

    def nextState1(ix: Int, iy: Int): Option[Char] =
      seat(ix,iy) match {
        case Some('L') if count1(ix,iy) == 0 => Some('#')
        case Some('#') if count1(ix,iy) >= 4 => Some('L')
        case unchanged                      => unchanged
      }

    def nextState2(ix: Int, iy: Int): Option[Char] =
      seat(ix,iy) match {
        case Some('L') if count2(ix,iy) == 0 => Some('#')
        case Some('#') if count2(ix,iy) >= 5 => Some('L')
        case unchanged                      => unchanged
      }
  
    def next1: Floor =
      Floor(
        (0 until floor.length).map { iy =>
          (0 until floor.head.length).map { ix =>
            nextState1(ix,iy).getOrElse('.')
          }.mkString
        }.toList
      )

    def next2: Floor =
      Floor(
        (0 until floor.length).map { iy =>
          (0 until floor.head.length).map { ix =>
            nextState2(ix,iy).getOrElse('.')
          }.mkString
        }.toList
      )
  
    def totalOccupied: Int =
      (0 until floor.length).map { iy =>
        (0 until floor.head.length).map { ix =>
          seat(ix,iy) match {
            case Some('#') => 1
            case _ => 0
          }
        }.toList
      }.toList
      .map(_.sum).sum
  
    override def toString: String =
      floor.mkString("\n","\n","")
  }

  val floor: Floor =
    Floor(
      Source
        .fromFile("src/resources/input11.txt")
        .getLines
        .toList)

  val start1 = System.currentTimeMillis

  val answer1: Int = {
    def loop(f: Floor): Floor = {
      val n = f.next1
      if (n == f) f else loop(n)
    }
    loop(floor).totalOccupied
  }

  println(s"Answer part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2 = System.currentTimeMillis

  val answer2: Int = {
    def loop(f: Floor): Floor = {
      val n = f.next2
      if (n == f) f else loop(n)
    }
    loop(floor).totalOccupied
  }

  println(s"Answer part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")