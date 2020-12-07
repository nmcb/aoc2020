import scala.io._

object Day07 extends App {

  val Line = """(\w+)\s(\w+)\sbags\scontain\s(.+).""".r
  val Bags = """(\d+)\s(\w+)\s(\w+)\sbag[s]?""".r

  case class Bag(from: String, to: Option[String], count: Int)

  val input: List[String] =
    Source
      .fromFile("src/resources/input07.txt")
      .getLines
      .toList

  val bags: List[Bag] =
    input.map {
      case Line(from1, from2, contains) => contains.split(',').map(_.trim).map {
          case Bags(count, to1, to2) => Bag(from1 + from2, Some(to1 + to2), count.toInt)
          case _                     => Bag(from1 + from2, None, 0)
        }
    }.flatten

  def parents(to: String): List[String] =
    bags.filter(b => b.to == Some(to)).map(_.from)

  def answer1(ps: List[String], roots: List[String] = Nil): List[String] =
    if (ps.isEmpty)
      roots
    else {
      val end = ps.filter(p => parents(p).isEmpty)
      val rec = ps.filter(p => parents(p).nonEmpty)
      answer1(rec.map(p => parents(p)).flatten.distinct, roots ++ rec ++ end)
    } 

  def children(from: String): List[(String,Int)] =
    bags.filter(b => b.from == from && b.to != None).map(b => b.to.get -> b.count)

  def answer2(inner: List[(String,Int)], acc: Int = 0): Int = {
      inner match {
        case Nil                 => acc
        case (child,count) :: cs => answer2(cs, acc + count + count * answer2(children(child)))
      }
    }

  // println(s"Answer part 1: ${parents("shinygold")}")
  println(s"Answer part 1: ${answer1(parents("shinygold")).distinct.size}")
  println(s"Answer part 2: ${answer2(children("shinygold"))}")
}
