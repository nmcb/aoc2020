import scala.io._
import scala.annotation._

object Day20 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val Header = "Tile\\s(\\d+):".r

  val inputs: List[Tile] =
    Source
      .fromResource(s"input$day-example1.txt")
      .getLines
      .map(_.trim)
      .filter(_.nonEmpty)
      .toList
      .foldLeft(List.empty[(Long,List[String])]) {
        case (acc, Header(d)) => // -> Tile.id
          acc :+ (d.toLong -> List.empty[String])
        case (acc, line) => // -> Tile.img :+ line
          acc.init :+ (acc.last._1 -> (acc.last._2 :+ line))
      }
      .map((id,img) => Piece(id,img))
      .toList

  def answer1(input: List[Tile]): Long = {

    def ids: List[Long] =
      input.map(_.id)
  
    def tile(id: Long): Tile =
      input.find(_.id == id).get
  
    val fits: List[(Long,Long,Set[String])] = 
      (for { 
        s <- input
        t <- input if s != t && s.fitsOn(t).nonEmpty
      } yield (s.id, t.id, s.fitsOn(t)))
  
    val corners: List[Corner] =
      input
        .foldLeft(List.empty[Corner])((acc,tile) =>
          if (fits.count(_._1 == tile.id) != 2) then acc else {
            val Seq(fit1,fit2) = fits.filter(_._1 == tile.id).map(_._3.head).toSeq
            acc :+ Corner(tile.id, tile.img, fit1, fit2)
          })

    corners.map(_.id).product
  }

  val start1 = System.currentTimeMillis
  println(s"Answer part 1: \n${answer1(inputs)} [${System.currentTimeMillis - start1}ms]")

  sealed trait Tile {

    val id:  Long
    val img: List[String]
    val oriented: Option[Orientation]
  
    val N: String = img.head
    val E: String = img.foldLeft("")((acc,row) => acc + row.last)
    val S: String = img.last
    val W: String = img.foldLeft("")((acc,col) => acc + col.head)

    def fits: Set[String] =
      Set(N, N.reverse, E, E.reverse, S, S.reverse, W, W.reverse)

    def fitsOn(that: Tile): Set[String] =
      fits intersect that.fits

    def mkString: String =
      s"""Tile $id:
          |$N - n
          |$E - e
          |$S - s
          |$W - w
          |----------
          |${img.mkString("\n")}
          """.stripMargin
  }

  case class Piece( val id: Long
                  , val img: List[String]
                  , oriented: Option[Orientation] = None
                  )
    extends Tile
  {
    def flip: Tile =
      copy(img = img.map(_.reverse))
  }

  case class Corner( override val id: Long
                   , override val img: List[String]
                   , fit1: String
                   , fit2: String
                   , oriented: Option[Orientation] = None
                   )
    extends Tile
  {
    import Orientation._

    override def fits: Set[String] =
      Set(fit1, fit2)

    def orient: Corner = {
      assert(oriented == None)
      // println(s"orienting ... \n$mkString")
      def fit1sided(dir: String): Boolean    = dir == fit1 || dir == fit1.reverse
      def fit2sided(dir: String): Boolean    = dir == fit2 || dir == fit2.reverse
      def fit1reversed(fit: String): Boolean = fit == fit1.reverse
      def fit2reversed(fit: String): Boolean = fit == fit2.reverse
      (fit1sided(N), fit1sided(S), fit2sided(E), fit2sided(W)) match {
        
        case (  true, false,  true, false) if fit1reversed(N) && fit2reversed(E) =>
          copy(img = img.reverse.map(_.reverse), fit1 = fit1.reverse, fit2 = fit2.reverse, oriented = Some(NE))
        case (  true, false,  true, false) if fit1reversed(N) =>
          copy(img = img.map(_.reverse), fit1 = fit1.reverse, oriented = Some(SW))
        case (  true, false,  true, false) if fit2reversed(E) =>
          copy(img = img.reverse, fit2 = fit2.reverse, oriented = Some(NW))
        case (  true, false,  true, false) =>
          copy(oriented = Some(SW))

        case (  true, false, false,  true) if fit1reversed(N) && fit2reversed(W) =>
          copy(img = img.reverse.map(_.reverse), fit1 = fit1.reverse, fit2 = fit2.reverse, oriented = Some(NW))
        case (  true, false, false,  true) if fit1reversed(N) =>
          copy(img = img.map(_.reverse), fit1 = fit1.reverse, oriented = Some(SE))
        case (  true, false, false,  true) if fit2reversed(W) =>
          copy(img = img.reverse, fit2 = fit2.reverse, oriented = Some(NE))
        case (  true, false, false,  true) =>
          copy(oriented = Some(SE))

        case ( false,  true,  true, false) if fit1reversed(N) && fit2reversed(E) =>
          copy(img = img.reverse.map(_.reverse), fit1 = fit1.reverse, fit2 = fit2.reverse, oriented = Some(SE))
        case ( false,  true,  true, false) if fit1reversed(N) =>
          copy(img = img.map(_.reverse), fit1 = fit1.reverse, oriented = Some(NE))
        case ( false,  true,  true, false) if fit2reversed(E) =>
          copy(img = img.reverse, fit2 = fit2.reverse, oriented = Some(SW))
        case ( false,  true,  true, false) =>
          copy(oriented = Some(NW))

        case ( false,  true, false,  true) if fit1reversed(N) && fit2reversed(W) =>
          copy(img = img.reverse.map(_.reverse), fit1 = fit1.reverse, fit2 = fit2.reverse, oriented = Some(SW))
        case ( false,  true, false,  true) if fit1reversed(N) =>
          copy(img = img.map(_.reverse), fit1 = fit1.reverse, oriented = Some(NW))
        case ( false,  true, false,  true) if fit2reversed(W) =>
          copy(img = img.reverse, fit2 = fit2.reverse, oriented = Some(SE))
        case ( false,  true, false,  true) =>
          copy(oriented = Some(NE))

        case ( false, false, false, false) =>
          copy(fit1 = fit2, fit2 = fit1).orient

        case err =>
          sys.error(s"orienting $err: $mkString")
      }
    }

    override def mkString: String =
      s"""Tile $id:
         |n=$N
         |e=$E
         |s=$S
         |w=$W
         |fit1=$fit1
         |fit2=$fit2
         |----------
         |oriented=${oriented.map(_.toString)getOrElse("<no>")}
         |----------
         |${img.mkString("\n")}
         """.stripMargin
  }

  enum Orientation {
    case NW
    case NE
    case SE
    case SW
  } 

  case class Cross( placed: Map[Orientation,Tile] = Map.empty
                  , on: Option[String] = Option.empty
                  , os: Option[String] = Option.empty
                  , oe: Option[String] = Option.empty
                  , ow: Option[String] = Option.empty
                  )
  {
    import Orientation._

    def fitsNW(t: Tile): Boolean = fits(NW)(t)
    def fitsNE(t: Tile): Boolean = fits(NE)(t)
    def fitsSW(t: Tile): Boolean = fits(SW)(t)
    def fitsSE(t: Tile): Boolean = fits(SE)(t)

    def fits(o: Orientation)(t: Tile): Boolean =
      o match {
        case NW => Some(t.S) == oe && Some(t.E) == on
        case NE => Some(t.S) == ow && Some(t.E) == on
        case SW => Some(t.N) == oe && Some(t.W) == os
        case SE => Some(t.N) == ow && Some(t.W) == os
      }

    def mkString: String =
      s"""Cross ${placed.map((d,t) => s"$d=${t.id}").mkString(", ")}:
         |on=${on.map(_.toString).getOrElse("<free>")}
         |os=${os.map(_.toString).getOrElse("<free>")}
         |oe=${oe.map(_.toString).getOrElse("<free>")}
         |ow=${ow.map(_.toString).getOrElse("<free>")}
         |""".stripMargin
  }

  object Cross {
    import Orientation._
    def from(c: Corner): Cross =
      c.orient.oriented match {
        case Some(NW) => Cross(placed = Map(NW -> c), on = Some(c.E), ow = Some(c.S))
        case Some(NE) => Cross(placed = Map(NE -> c), on = Some(c.W), oe = Some(c.S))
        case Some(SW) => Cross(placed = Map(SW -> c), os = Some(c.E), ow = Some(c.N))
        case Some(SE) => Cross(placed = Map(SE -> c), os = Some(c.W), oe = Some(c.N))
        case _ => sys.error(s"from corner c=${c.mkString}") 
      }
  }

  def solve( input: List[Tile]     = inputs
           , acc: List[List[Tile]] = List.empty
           ): List[List[Tile]] = {
    import math._
    assert(sqrt(input.length) == floor(sqrt(input.length)))

    ???
  }
