import scala.io._

object Day04 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  type Passport = Map[String,String]

  object Passport:
    val empty: Passport =
      Map.empty[String,String]

  val input =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .toList

  val passports: Vector[Passport] =
    input.foldLeft(Vector(Passport.empty)): (acc,line) =>
      line.split(' ') match
        case Array("") =>
          Passport.empty +: acc
        case line: Array[String] =>
          val update = line.map(_.split(':')).map(kv => kv(0) -> kv(1)).toMap
          (update ++ acc.head) +: acc.tail

  def valYear(key: String, min: Int, max: Int)(p: Passport): Boolean =
    p.get(key) match {
      case Some(str) =>
        str.toInt >= min && str.toInt <= max
      case _ =>
        false
    }
    
  def valHgt(p: Passport): Boolean =
    p.get("hgt") match {
      case Some(ht) =>
        val h = ht.takeWhile(_.isDigit).toInt
        if      (ht.endsWith("cm"))  h >= 150 && h <= 193
        else if (ht.endsWith("in"))  h >=  59 && h <=  76
        else                         false
      case _ =>
        false
    }

  def valHcl(p: Passport): Boolean = {
    val chars = "0123456789abcdef"
    p.get("hcl") match {
      case Some(hc) if hc.startsWith("#") =>
        hc.drop(1).filter(chars.contains).size == 6
      case _ =>
        false
    }
  }
          
  def valEcl(p: Passport): Boolean = {
    val colors = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    p.get("ecl") match {
      case Some(col) =>
        colors.contains(col)
      case None =>
        false
    }
  }
          
  def valPid(p: Passport): Boolean = {
    val digits = "0123456789"
    p.get("pid") match {
      case Some(id) =>
        id.filter(digits.contains).size == 9
      case _ =>
        false
    }
  }

  def solve1(passports: Vector[Passport]): Vector[Passport] =
    passports.filter(p => p.size == 8 || (p.size == 7 && !p.keys.exists(_  == "cid")))


  val start1  = System.currentTimeMillis
  val answer1 = solve1(passports).size
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def solve2(passports: Vector[Passport]): Vector[Passport] =
    solve1(passports)
      .filter(valYear("byr", 1920, 2020))
      .filter(valYear("iyr", 2010, 2020))
      .filter(valYear("eyr", 2020, 2030))
      .filter(valHgt)
      .filter(valHcl)
      .filter(valEcl)
      .filter(valPid)

  val start2  = System.currentTimeMillis
  val answer2 = solve2(passports).size
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
