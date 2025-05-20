import scala.annotation.tailrec

object Day23 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  /** a singe linked, updatable list of cups */
  case class Cup(value: Int, var next: Cup = null)

  def play(digits: Vector[Int], max: Int): Cup =

    import Iterator.*
    val cups = digits
        .scanRight(0 -> Cup(0)):
          case (current, (value, next)) => current -> Cup(current, next)
        .toMap
    cups(digits.last).next = cups(digits.head)

    @tailrec
    def go(cup: Cup, step: Int): Cup =
      if step == max then
        cups(1)
      else
        val a = cup.next
        val b = a.next
        val c = b.next
        val picked = Set(cup.value, a.value, b.value, c.value)
        def next(n: Int) = if n == 1 then digits.size else n - 1
        val target = iterate(cup.value)(next).dropWhile(picked.contains).next

        cup.next = c.next
        c.next   = cups(target).next
        cups(target).next = a
        go(cup.next, step + 1)
    go(cups(digits.head), 0)

  def solve1(digits: Vector[Int]): Long =
    val start = play(digits, 100)

    @tailrec
    def go(cup: Cup, seq: Vector[Int]): Long =
      if cup == start then seq.mkString.toLong else go(cup.next, seq :+ cup.value)

    go(start.next, Vector.empty)

  def solve2(digits: Vector[Int]): Long =
    val seq = digits ++ (10 to 1000000)
    val cup = play(seq, 10000000)
    val (a, b) = (cup.next.value, cup.next.next.value)
    a.toLong * b.toLong

  val digits = "193467258".trim.map(_.asDigit).toVector

  val start1  = System.currentTimeMillis
  val answer1 = solve1(digits)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = solve2(digits)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
