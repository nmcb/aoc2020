import scala.annotation.tailrec
import scala.io.Source

object Day22 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  enum Winner(val deck: Vector[Int]):
    case Player1(override val deck: Vector[Int]) extends Winner(deck)
    case Player2(override val deck: Vector[Int]) extends Winner(deck)

    def score: Int =

      extension (card: (Int, Int))
        def value: Int     = card._1
        def multplier: Int = card._2

      (deck :+ 0)
        .reverse
        .zipWithIndex
        .foldLeft(0)((score, card) => score + card.value * card.multplier)

  import Winner.*

  case class Game(deck1: Vector[Int], deck2: Vector[Int]):

    def player1Wins: Game =
      Game(deck1.tail ++ Seq(deck1.head, deck2.head), deck2.tail)

    def player2Wins: Game =
      Game(deck1.tail, deck2.tail ++ Seq(deck2.head, deck1.head))

    @tailrec
    final def combat: Winner =
      if      deck2.isEmpty then Player1(deck1)
      else if deck1.isEmpty then Player2(deck2)
      else (if deck1.head > deck2.head then player1Wins else player2Wins).combat

    def recursiveCombat: Winner =
      def go(round: Game, cache: Set[Game] = Set.empty): Winner =
        round match
          case Game(deck1, Vector()) => Player1(deck1)
          case Game(Vector(), deck2) => Player2(deck2)
          case Game(deck1, deck2)    =>
            go( round = if cache.contains(round) then
                          Game(deck1, Vector.empty)
                        else if deck1.head > deck1.tail.size || deck2.head > deck2.tail.size then
                          if deck1.head > deck2.head then round.player1Wins else round.player2Wins
                        else
                          val d1 = deck1.tail.take(deck1.head)
                          val d2 = deck2.tail.take(deck2.head)
                          go(Game(d1, d2)) match
                            case Player1(_) => round.player1Wins
                            case Player2(_) => round.player2Wins
              , cache = cache + round)
      go(this)

  val game: Game =
    val input = Source.fromResource(s"input$day.txt").getLines.toVector
    val index = input.indexOf("")
    val deck1 = input.slice(1, index).map(_.toInt)
    val deck2 = input.drop(index + 2).map(_.toInt)
    Game(deck1, deck2)

  val start1  = System.currentTimeMillis
  val answer1 = game.combat.score
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = game.recursiveCombat.score
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
