import scala.io._

object Day19 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  type Rules    = Map[Int, Rule]
  type Messages = Seq[String]

  sealed trait Rule:
    /** @return the optional prefix length of this line that matches this rule */
    def prefix(line: String)(using rules: Rules): Option[Int]

  case class Letter(s: String) extends Rule:
    override def prefix(line: String)(using rules: Rules): Option[Int] =
      Option.when(line.take(1) == s)(1)

  case class Redirect(x: Int) extends Rule:
    override def prefix(line: String)(using rules: Rules): Option[Int] =
      rules(x).prefix(line)

  case class Sequence(x: Int, y: Int) extends Rule:
    override def prefix(line: String)(using rules: Rules): Option[Int] =
      rules(x).prefix(line).flatMap(length => rules(y).prefix(line.drop(length)).map(_ + length))

  case class TripleSequence(x: Int, y: Int, z: Int) extends Rule:
    override def prefix(line: String)(using rules: Rules): Option[Int] =
      Sequence(x, y).prefix(line).flatMap(length => rules(z).prefix(line.drop(length)).map(_ + length))

  case class SingleChoice(x: Int, y: Int) extends Rule:
    override def prefix(line: String)(using rules: Rules): Option[Int] =
      rules(x).prefix(line).orElse(rules(y).prefix(line))

  case class DoubleChoice(w: Int, x: Int, y: Int, z: Int) extends Rule:
    override def prefix(line: String)(using rules: Rules): Option[Int] =
      Sequence(w, x).prefix(line).orElse(Sequence(y, z).prefix(line))


  val (rules: Rules, messages: Messages) =

    object Parse:
      val Letter          = "\"(\\w)\"".r
      val Redirect        = "(\\d+)".r
      val Sequence        = "(\\d+) (\\d+)".r
      val TrripleSequence = "(\\d+) (\\d+) (\\d+)".r
      val SingleChoice    = "(\\d+) \\| (\\d+)".r
      val DoubleChoice    = "(\\d+) (\\d+) \\| (\\d+) (\\d+)".r

    def parseRules(input: Seq[String]): Rules =
      val rules = input.map: line =>
        val Array(id, pattern) = line.split(": ")
        val rule = pattern match
          case Parse.Letter(s)                => Letter(s)
          case Parse.Redirect(x)              => Redirect(x.toInt)
          case Parse.Sequence(x, y)           => Sequence(x.toInt, y.toInt)
          case Parse.TrripleSequence(x, y, z) => TripleSequence(x.toInt, y.toInt, z.toInt)
          case Parse.SingleChoice(x, y)       => SingleChoice(x.toInt, y.toInt)
          case Parse.DoubleChoice(w, x, y, z) => DoubleChoice(w.toInt, x.toInt, y.toInt, z.toInt)
        id.toInt -> rule
      rules.toMap

    val input    = Source.fromResource(s"input$day.txt").getLines().toSeq
    val index    = input.indexOf("")
    val rules    = parseRules(input.take(index))
    val messages = input.drop(index + 1)
    (rules, messages)


  def solve(rules: Rules, messages: Messages): Int =
    messages.count(message => rules(0).prefix(message)(using rules).contains(message.length))

  val start1  = System.currentTimeMillis
  val answer1 = solve(rules, messages)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  case class RuleZero(size: Int) extends Rule:
    override def prefix(line: String)(using rules: Rules): Option[Int] =
      var index  = line.length - size
      var length = 3 * size
      var found  = false
      while length <= line.length && !found do
        val left  = line.take(index).grouped(size).forall(group => rules(42).prefix(group).contains(size))
        val right = line.drop(index).grouped(size).forall(group => rules(31).prefix(group).contains(size))
        index  -= size
        length += 2 * size
        found   = left && right
      Option.when(found)(line.length)

  val start2  = System.currentTimeMillis
  val answer2 = solve(rules.updated(0, RuleZero(8)), messages)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

