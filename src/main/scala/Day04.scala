import scala.io._

object Day04 {

  val input =
      Source
        .fromFile("src/resources/input04.txt")
        .getLines
        .toList

  def read(inp: List[String], acc: List[Map[String,String]] = List(Map.empty)): List[Map[String,String]] = {
    if (inp.isEmpty) {
      acc
    }
    else if (inp.head.trim.isEmpty) {
      read(inp.tail, Map.empty :: acc)
    }
    else {
      val line: Map[String,String] =
        inp.head.split(' ').filterNot(_.isEmpty).map(s => {
          println(s)
          val Array(k,v) = s.trim.split(':')
          k -> v
          }).toMap
  
      read(inp.tail, (acc.head ++ line) :: acc.tail)
    }
  }

  val answer1 =
    read(input).filter(p => p.size == 8 || (p.size == 7 && !p.keys.toList.contains("cid")))

  def valByr(p: Map[String,String]): Boolean =
    p.get("byr") match {
      case Some(yr) if (yr.toInt >= 1920 && yr.toInt <= 2002) => true
      case _ => false
    }

  def valIyr(p: Map[String,String]): Boolean =
    p.get("iyr") match {
      case Some(yr) if (yr.toInt >= 2010 && yr.toInt <= 2020) => true
      case _ => false
    }
  
  def valEyr(p: Map[String,String]): Boolean =
    p.get("eyr") match {
      case Some(yr) if (yr.toInt >= 2020 && yr.toInt <= 2030) => true
      case _ => false
    }
    
  def valHgt(p: Map[String,String]): Boolean =
    p.get("hgt") match {
      case Some(ht) => {
        if (ht.endsWith("cm")) {
          val h = ht.takeWhile(_ != 'c').toInt
          (h >= 150 && h <= 193)
        }
        else if (ht.endsWith("in")) {
          val h = ht.takeWhile(_ != 'i').toInt
          (h >= 59 && h <= 76)
        }
        else
          false 
      }
      case _ => false
    }

  val c = "0123456789abcdef"

  def valHcl(p: Map[String,String]): Boolean =
    p.get("hcl") match {
      case Some(hc) if (hc.startsWith("#") && hc.drop(1).filter(c.contains(_)).size == 6) => true
      case _ => false
    }
          
  def valEcl(p: Map[String,String]): Boolean =
    p.get("ecl") match {
      case Some("amb") => true
      case Some("blu") => true
      case Some("brn") => true
      case Some("gry") => true
      case Some("grn") => true
      case Some("hzl") => true
      case Some("oth") => true
      case _ => false
    }
          
  def valPid(p: Map[String,String]): Boolean =
    p.get("pid") match {
      case Some(id) if (id.filter("0123456789".contains(_)).size == 9) => true
      case _ => false
    }

  val answer2 = 
    answer1
      .filter(valByr)     
      .filter(valIyr)
      .filter(valEyr)
      .filter(valHgt)
      .filter(valHcl)
      .filter(valEcl)
      .filter(valPid)
    
          
  def main(args: Array[String]): Unit = {

    println(s"Answer part 1: ${answer1.size}")
    println(s"Answer part 2: ${answer2.size}")
  }
}
