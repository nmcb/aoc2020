import scala.io._
import scala.annotation._
import scala.util.control.Breaks._
import scala.util._
import scala.collection._

object Day15 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val input: List[(Int,(Int,Option[Int]))] =
    "5,1,9,18,13,8,0"
      .split(',')
      .zipWithIndex
      .map((nr,ix) => nr.toInt -> ((ix + 1),None))
      .toList

  val answer: Int => Int = {
    @tailrec def solve(state: Map[Int,(Int,Option[Int])], turn: Int, last: Int)(limit: Int): Int =
      if (turn > limit)
        last
      else {
        val next   = turn + 1
        val spoken = state.get(last) match {
          case Some((  _ , None     )) => 0
          case Some(( t1 , Some(t2) )) => t1 - t2
          case None => ???
        }
        val updateStateSpoken = state.get(spoken) match {
          case Some(( t1 , _ )) => (turn, Some(t1))
          case None             => (turn, Option.empty[Int])
        }
        val nextSt = state ++ Map(spoken -> updateStateSpoken)
        solve(nextSt, next, spoken)(limit)
      }
    solve(input.toMap, input.length + 1, input.last._1)
  }

  val start1  = System.currentTimeMillis
  println(s"Answer part 1: ${answer(2020)} [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  println(s"Answer part 2: ${answer(30000000)} [${System.currentTimeMillis - start2}ms]")