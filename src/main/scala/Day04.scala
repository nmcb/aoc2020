import scala.io._

object Day04 {

  type Passport = Map[String,String]
  val  Passport = Map.empty[String,String]

  val input =
      Source
        .fromFile("src/resources/input04.txt")
        .getLines
        .toList

  val passports: List[Passport] =
    input.foldLeft(List(Passport)) { (acc,line) =>
      line.split(' ') match {
        case Array("")         =>
          Passport :: acc
        case fs: Array[String] =>
          (fs.map(_.split(':')).map(kv => kv(0) -> kv(1)).toMap ++ acc.head) :: acc.tail
      }
    }

  val answer1 =
    passports.filter(p => p.size == 8 || (p.size == 7 && !p.keys.toList.contains("cid")))

  def valRange(key: String, min: Int, max: Int)(p: Passport): Boolean =
    p.get(key) match {
      case Some(str) if (str.toInt >= min && str.toInt <= max) => true
      case _ => false
    }
    
  def valHgt(p: Passport): Boolean =
    p.get("hgt") match {
      case Some(ht) if ht.endsWith("cm") =>
        val h = ht.takeWhile(_ != 'c').toInt
        h >= 150 && h <= 193
      case Some(ht) if ht.endsWith("in") =>
        val h = ht.takeWhile(_ != 'i').toInt
        h >= 59 && h <= 76
      case _ =>
        false
    }


  def valHcl(p: Passport): Boolean = {
    val chars = "0123456789abcdef"
    p.get("hcl") match {
      case Some(hc) =>
        hc.startsWith("#") && hc.drop(1).filter(chars.contains(_)).size == 6
      case None =>
        false
    }
  }
          
  def valEcl(p: Passport): Boolean = {
    val colors = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    p.get("ecl") match {
      case Some(col) =>
        colors.contains(col)
      case None      =>
        false
    }
  }
          
  def valPid(p: Passport): Boolean = {
    val digits = "0123456789"
    p.get("pid") match {
      case Some(id) =>
        id.filter(digits.contains(_)).size == 9
      case _ =>
        false
    }
  }

  val answer2 = 
    answer1
      .filter(valRange("byr", 1920, 2020))     
      .filter(valRange("iyr", 2010, 2020))
      .filter(valRange("eyr", 2020, 2030))
      .filter(valHgt)
      .filter(valHcl)
      .filter(valEcl)
      .filter(valPid)
    
          
  def main(args: Array[String]): Unit = {

    println(s"Answer part 1: ${answer1.size}")
    println(s"Answer part 2: ${answer2.size}")
  }
}
