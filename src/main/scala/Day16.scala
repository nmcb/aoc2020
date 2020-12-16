import scala.io._
import scala.annotation._
import scala.util.control.Breaks._
import scala.util._
import scala.collection._

object Day16 extends App {

  case class Rule(label: String, a0: Int, a1: Int, b0: Int, b1: Int) {
    def valid(i: Int): Boolean =
      (i >= a0 && i <= a1) || (i >= b0 && i <= b1)
    def invalid(i: Int): Boolean =
      !valid(i)
  }

  val Rules = "(.+)\\: (\\d+)-(\\d+) or (\\d+)-(\\d+)".r

  val Input = "src/resources/input16.txt"

  val rules: List[Rule] =
    Source
      .fromFile(Input)
      .getLines
      .foldLeft(List.empty[Rule]) {
        case (rs, Rules(l, a0,a1,b0,b1)) =>
          rs :+ Rule(l, a0.toInt, a1.toInt, b0.toInt, b1.toInt)
        case (rs, _) => rs
      }

  val RuleCount = rules.length

  val ticket: List[Int] =
    Source
        .fromFile(Input)
        .getLines
        .drop(rules.length + 2)
        .next
        .split(',')
        .map(_.toInt)
        .toList

  val nearby: List[List[Int]] =
    Source
        .fromFile(Input)
        .getLines
        .drop(rules.length + 5)
        .toList
        .map(_.split(',').map(_.toInt).toList)

  val start1  = System.currentTimeMillis

  def invalid(t: List[Int]): List[Int] =
    t.foldLeft(List.empty[Int])((a,i) => if (rules.forall(_.invalid(i))) i :: a else a)

  val (validTickets, answer1): Tuple2[List[List[Int]],Int] = {
    nearby.foldLeft((List.empty[List[Int]],0)) {
      case ((vs,a),t) => {
        val invalidNrs = invalid(t)
        if (invalidNrs.nonEmpty)
          (vs, a * invalidNrs.sum)
        else
          (t :: vs, a)
      }
    }
  }

  println(s"Answer part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis

  val rulesByLengthAndLabel: List[(List[Rule],Int)] =
    (0 until RuleCount).foldLeft(List.empty[List[Rule]])((acc,i) =>
      acc :+ validTickets.foldLeft(rules)((r,t) =>
        r.filter(_.valid(t(i)))
      ).sortBy(_.label)
    ).zipWithIndex.sortBy(_._1.length)

  @tailrec def purge(todo: List[(List[Rule],Int)], acc: List[(Rule,Int)] = List.empty): List[(Rule,Int)] = todo match {
    case (rs, ix) :: rss =>
      val r    = rs.headOption.getOrElse[Rule](sys.error("empty ruleset"))
      val next = rss.map((rs,i) => (rs.filterNot(_ == r), i))
      purge(next, acc :+ (r -> ix))
    case Nil =>
      acc.sortBy(_._2)
  }

  val departureRules =
    purge(rulesByLengthAndLabel).filter(_._1.label.startsWith("departure"))

  val departureFields =
    departureRules.map((_,i) => ticket(i).toLong)

  println(s"Answer part 2: ${departureFields.product} [${System.currentTimeMillis - start1}ms]")
}