import scala.annotation.tailrec

object Day22 extends App {

  type Deck = List[Int]
  val  Deck = List

  case class Win(player1: Option[Deck] = None, player2: Option[Deck] = None) {

    def byPlayer1: Boolean =
      player1.nonEmpty

    def byPlayer2: Boolean =
      player2.nonEmpty
  
    def deck: Deck =
      player1.getOrElse(player2.getOrElse(sys.error("no win")))

    def score: Long =
      deck
        .reverse
        .zipWithIndex
        .map((n,i) => (n,i+1))
        .foldRight(0L) { case ((nr,ix),ac) => (nr * ix) + ac }
  }

  object Win {
    def empty: Win = Win()
  }

  val example1: Deck =
    Deck(9, 2, 6, 3,  1)

  val example2: Deck =
    Deck(5, 8, 4, 7, 10)

  val input1: Deck =
    Deck( 17, 19, 30, 45, 25, 48,  8,  6, 39, 36
        , 28,  5, 47, 26, 46, 20, 18, 13,  7, 49
        , 34, 23, 43, 22,  4
        )

  val input2: Deck = 
    Deck( 44, 10, 27,  9, 14, 15, 24, 16,  3, 33
        , 21, 29, 11, 38,  1, 31, 50, 41, 40, 32
        , 42, 35, 37,  2, 12
        )

  val start1: Long =
    System.currentTimeMillis

  val answer1: Long = {
    @tailrec def play(as: Deck, bs: Deck): Win = {
      (as, bs) match {
        case (a +: atail, b +: btail) =>
          if (a > b)
            play(atail :+ a :+ b, btail)
          else
            play(atail, btail :+ b :+ a)

        case (_, Nil) => Win(player1 = Some(as))
        case (Nil, _) => Win(player2 = Some(bs))
        case _        => sys.error("both empty")
      }
    }    
    val result = play(input1, input2)
    result.score
  }

  println(s"Answer part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  /*
  from collections import deque
  from math import prod

  with open('input') as f:
  ls = [line.strip() for line in f.readlines()]

  p1 = deque(map(int, ls[1:26]))
  p2 = deque(map(int, ls[28:]))
  
  def score(winner):
    return sum(map(prod, enumerate(reversed(winner), 1)))
  */

  /*
  def game(p1, p2):
    seen = set()
    while p1 and p2:
        conf = (tuple(p1), tuple(p2))

        if conf in seen:
            return True, p1

        seen.add(conf)
        c1 = p1.popleft()
        c2 = p2.popleft()

        if len(p1) >= c1 and len(p2) >= c2:
            p1_won, _ = game(deque(list(p1)[:c1]),
                            deque(list(p2)[:c2]))
        else:
            p1_won = c1 > c2

        if p1_won:
            p1 += [c1, c2]
        else:
            p2 += [c2, c1]
    return p1_won, p1 or p2

  _, winner = game(deque(p1), deque(p2))
  print(score(winner))
  */

  val start2: Long = System.currentTimeMillis

  val answer2: Long = {
    def play(as: Deck, bs: Deck, seen: Set[(Deck,Deck)] = Set.empty, round: Int = 1): Win = {
      println(s"-- Round $round --")
      println(s"Player 1's deck: $as")
      println(s"Player 2's deck: $bs") 
      (as,bs) match {
        case (a +: atail, b +: btail) =>
          val current = (as,bs)
          if (seen.contains(current))
            Win(player1 = Some(as))
          else {
            if (as.length >= a && bs.length >= b)
              play(as.take(a), bs.take(b), seen + current)
            else if (a > b)
              play(atail :+ a :+ b, btail, seen, round + 1)
            else
              Win(player2 = Some(btail :+ b :+ a))
          }
        case (_, Nil) => Win(player1 = Some(as))
        case (Nil, _) => Win(player2 = Some(bs))
        case _        => sys.error("both empty")
      }
    }
    play(example1,example2).score
  }

  println(s"Answer part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

}