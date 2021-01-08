import scala.annotation._

object Day23 extends App {


  case class Round(ring: List[Int], move: Int = 1) {
    val size = 9
    assert(ring.length == size)

    val current: Int =
      ring(0)

    def peek(idx: Int): Int =
      ring(idx % 9)

    def find(i: Int): Int =
      ring.indexOf(i)

    val pickup: List[Int] = 
      ring.drop(1).take(3)

    val destination: Int = {
      def loop(n: Int): Int =
        if (n == 0)
          loop(size)
        else if (!pickup.contains(n))
           n
        else
          loop(n - 1)
      loop(current - 1)
    }

    def next: Round = {
      def cycle(i: Int): Round = {
        val test = ring.drop(4) :+ current
        println(s"test=$test")
        val ncur = test.head
        println(s"ncur=$ncur")               
        val res =
          if (ncur == destination)
            val tail = test.drop(1)
            println(s"tail=$tail")
            ncur +: pickup :++ tail
          else
            val init = test.drop(1).takeWhile(_ != i)
            println(s"init=$init")
            val tail = test.drop(1).dropWhile(_ != i).drop(1)
            println(s"tail=$tail")
            ncur +: init :+ destination :++ pickup :++ tail
        println(s"res=$res")
        Round(res,move+1)
      }
      cycle(destination)
    }

    override def toString: String =
      s"""-- move $move --
         |cups: $ring
         |current: $current
         |pick up: [${pickup.mkString(" ")}]
         |destination: $destination
      """
  }

  object Round {
    def apply(digits: String): Round =
      Round(digits.map(_.toString.toInt).toList)
  }
  

  val start1 = System.currentTimeMillis

  val answer1: Unit = {
    def play(round: Round): Unit =
      if (round.move <= 100)
        println(s"$round")
        play(round.next)
      else 
        println(s"$round")
    // play(Round("389125467")) // CRAB
    play(Round("193467258")) // MINE
  }

  println(s"Answer part 2: ${answer1} [${System.currentTimeMillis - start1}ms]")


  val answer2: Unit = {
    def play(round: Round): Unit =
      if (round.move <= 100)
        println(s"$round")
        play(round.next)
      else 
        println(s"$round")
    // play(Round("389125467")) // CRAB
    play(Round("193467258")) // MINE
  }

  // 1000000 -> ???
}