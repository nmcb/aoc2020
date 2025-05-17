import scala.io._

object Day21 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  def partTwo = {
    val x =
      Source
        .fromResource(s"input$day.txt").getLines
        .foldLeft(Map.empty[String, Set[String]]) { case (acc, line) =>
      val split = line.split(" ").toList
      val ingredients = split.takeWhile(_ != "(contains")
      val allergenz = split.drop(ingredients.length + 1).map(_.replace(")", "").replace(",", ""))
      val map = allergenz.map { all =>
        if (acc.contains(all)) {
          (all, acc(all).intersect(ingredients.toSet))
        } else {
          (all, ingredients.toSet)
        }
      }.toMap
      acc ++ map
    }
    var toxic = Map.empty[String, String]
    var y = x
    var i = 0
    while(!y.isEmpty) {
      println(y.toList.head)
      i = if (i >= y.size) 0 else i
      val (k, vals) = y.toList(i)
      if (vals.size == 1 && !toxic.contains(k)) {
        toxic = toxic.updated(k, vals.head)
        println(s"Removing $k from the list")
        y = y - k
        for (k <- y) {
          y = y.updated(k._1, y(k._1) - vals.head)
        }
      }
      i += 1
    }
    println(toxic.toList.sortBy(_._1).map(_._2).mkString(","))
  }

  val start2 = System.currentTimeMillis
  println(s"Answer part 2: ${partTwo} [${System.currentTimeMillis - start2}ms]")
