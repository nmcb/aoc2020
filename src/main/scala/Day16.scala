import scala.io.*

object Day16 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Rule(pattern: String):

    private val (a, b, c, d) = pattern match
      case s"${name}: $a-$b or $c-$d" => (a.toInt, b.toInt, c.toInt, d.toInt)

    def check(i: Int): Boolean = (a <= i && i <= b) || (c <= i && i <= d)

  val (rules: Set[Rule], mine: Seq[Int], nearby: Seq[Seq[Int]]) =
    val input = Source.fromResource(s"input$day.txt").getLines.toSeq
    val (rules, rest)  = input.splitAt(input.indexOf(""))
    val (your, nearby) = (rest(2), rest.drop(5))
    (rules.map(Rule.apply).toSet, your.split(",").map(_.toInt).toSeq, nearby.map(_.split(",").map(_.toInt).toSeq))

  def solve1(rules: Set[Rule], mine: Seq[Int], nearby: Seq[Seq[Int]]): Long =
    nearby.flatMap(ticket => ticket.filterNot(field => rules.exists(rule => rule.check(field)))).sum

  val start1  = System.currentTimeMillis
  val answer1 = solve1(rules, mine, nearby)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def solve2(rules: Set[Rule], mine: Seq[Int], nearby: Seq[Seq[Int]]): Long =
    val valid = nearby.filter(ticket => ticket.forall(field => rules.exists(rule => rule.check(field))))
    val sets  = valid.transpose.map(fields => rules.filter(rule => fields.forall(field => rule.check(field))))

    def refine(sets: Seq[Set[Rule]]): Seq[Set[Rule]] =
      val definite = sets.filter(_.size == 1).flatten
      sets.map(set => if set.size == 1 then set else set -- definite)

    val found = Iterator.iterate(sets)(refine).dropWhile(_.exists(_.size > 1)).next.map(_.head)
    val departures = found.zip(mine).filter((rule,field) => rule.pattern.startsWith("departure"))
    departures.map((_,field) => field.toLong).product

  val start2  = System.currentTimeMillis
  val answer2 = solve2(rules, mine, nearby)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
