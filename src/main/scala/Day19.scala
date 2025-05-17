import scala.io._
import scala.collection._
import scala.annotation._
import scala.util.Try

object Day19 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  import parsing._
  import P._

  sealed trait Rule {
    def genp: P[String] = ???
    def matches(s: String): (Boolean,String) = ???
  }
  case class Ref(idx: Rule , rhs: Rule) extends Rule {
    override def genp: P[String] =
      rhs.genp
    override def matches(s: String): (Boolean,String) =
      println(s"idx=$idx -> $s")
      if (s.isEmpty) (true,"") else {
        val res = rhs.matches(s)
        res
      }
  }
  case class Num(v: Long) extends Rule {
    override def genp: P[String] =
      P(s => rules(v).genp.parse(s))
    override def matches(s: String): (Boolean,String) =
      rules(v).matches(s)
  }
  case class Or (lhs: Rule , rhs: Rule) extends Rule {
    override def genp: P[String] =
      P(s => {
        lhs.genp.parse(s) match {
          case Some(lv,r1) => Some(lv,r1)
          case None => rhs.genp.parse(s) match {
            case Some(rv,r2) => Some(rv,r2)
            case None => None
          }
        }
      })
    override def matches(s: String): (Boolean,String) =
      if (s.isEmpty) (true,"") else lhs.matches(s) match {
        case (true,r) => (true,r)
        case (false,_) => rhs.matches(s)
      }
  }
  case class And(lhs: Rule, rhs: Rule) extends Rule {
    override def genp: P[String] =
      P(s => {
        for {
          (lc,r1) <- lhs.genp.parse(s)
          (rc,r2) <- rhs.genp.parse(r1)
        } yield (lc+rc, r2)
      })

    override def matches(s: String): (Boolean,String) =
      if (s.isEmpty) (true,"") else lhs.matches(s) match {
        case (true,r) => rhs.matches(r)
        case (false,_) => (false,s)
      }
  }
  case class Lit(c: Char) extends Rule {
    override def genp: P[String] =
      char(c).map(c => {
        c.toString
      })
    override def matches(s: String): (Boolean,String) =
      if (s.isEmpty) then {
        (true,"")
      }
      else if (s.head == c) then {
        println(s"take=$c")
        (true,s.tail)
      }
      else {
        (false,s)
      }
  }

  object Rule {

    // ref  := lhs@rule -> { ": " rhs@rule }  => Ref(lhs,rhs)
    // rule := or | and | num | lit
    // or   := lhs@and -> { " | " rhs@and }   => Or(lhs,rhs)
    // and  := lhs@num -> { " " rhs@num }     => And(lhs,rhs)
    // num  := digit -> { digit }             => Num(num)
    // lit  := '"' char@take '"'              => Lit(char)

    type BinOp = Rule => Rule => Rule

    def infix(op: String)(f: BinOp): P[BinOp] =
      keyword(op) ~ unit(f)

    def ref: P[Rule] =
      rule.chainl1(infix(": ")(lhs => rhs => Ref(lhs,rhs)))

    def rule: P[Rule] =
      or | and | num | lit

    def or: P[Rule] =
      and.chainl1(infix(" | ")(lhs => rhs => Or(lhs,rhs)))

    def and: P[Rule] =
      num.chainl1(infix(" ")(lhs => rhs => And(lhs,rhs)))

    def num: P[Rule] =
      digits.map(l => Num(l))

    def lit: P[Rule] =
      for {
        _    <- char('"')
        c    <- take
        _    <- char('"')
      } yield Lit(c)

    def parse(s: String): Rule =
      run(Rule.ref)(s)
  }

  import Rule._

  val input =
    Source
      .fromFile("src/resources/input19example2.txt")
      .getLines
      .map(_.trim)
      .toList

  val rules: Map[Long,Ref] =
    input
      .takeWhile(_.length != 0)
      .map(parse)
      .map {
        case ref@Ref(Num(v), _) => v -> ref
        case _ => sys.error("boom")
      }
      .sortBy(_._1)
      .toMap

  println(s"rules.length${rules.keys.size}")

  val tests =
    input.dropWhile(_.length != 0).drop(1).zipWithIndex

  // println(tests.mkString("\n","\n",""))

  // val answer1 =
  //   tests.foreach(s => {
  //     val res = rules(0).rhs.matches(s._1)._1
  //     println(s"(${s._2}) $s [$res]")
  //   })

  def seen(rule: Rule, s: List[Long] = List.empty): (Long,List[Long]) = rule match {
    case Ref(Num(i),r) if s.contains(i) => (i,s)
    case Ref(Num(i),r)                  => seen(r,s :+ i)
    case Num(v)                         => seen(rules(v), s)
    case And(l,r) =>
      val (res,lc) = seen(l,s)
      seen(r,lc)
    case Or(l,r) =>
      val (res,lc) = seen(l,s)
      seen(r,lc)
    case Lit(c) => (-1,s)
    case r: Rule =>
      sys.error(s"unrecognised rule r=$r")
  }

  println(seen(rules(0)))

  // rules.foreach((idx,ref) => {
  //   println(str(ref))
  // })

  println(rules.mkString("\n","\n",""))

  // aaaabbaaaabbaaa
  // println(rules(0).rhs.matches("aaaabbaaaabbaaa")._1)

  // val start1 = System.currentTimeMillis
  // println(s"Answer part 1: \n${answer1} [${System.currentTimeMillis - start1}ms]")
