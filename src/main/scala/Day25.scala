object Day25 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  def solve(card: Int, door: Int): Int =
    def step(n: Int): Int = (n * 7) % 20201227
    BigInt(card).modPow(Iterator.iterate(1)(step).indexWhere(_ == door), 20201227).toInt

  val start1  = System.currentTimeMillis
  val answer1 = solve(card = 5290733, door = 15231938)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")
