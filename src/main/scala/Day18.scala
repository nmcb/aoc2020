import scala.io._
import scala.collection._

object Day18 extends App {

  val input =
      Source
        .fromFile("src/resources/input18.txt")
        .getLines
        .map(_.filter(_ != ' '))
        .toList

  sealed trait Expr
  case class Add(l: Expr, r: Expr) extends Expr
  case class Mul(l: Expr, r: Expr) extends Expr
  case class Val(v: Long) extends Expr

  def interpret(expr: Expr): Long =
    expr match {
      case Add(l,r) => interpret(l) + interpret(r)
      case Mul(l,r) => interpret(l) * interpret(r)
      case Val(v)   => v
    }

  import parsing._

  type Op = Expr => Expr => Expr

  def infix(op: String)(f: Op): P[Op] = 
    P.reserved(op) ~ P.unit(f)

  // expr1  := term2 -> { lassoc }      => Mul | Add
  // lassoc := '*' expr1 | '+' expr1
  // term1  := '('' expr1 ')' | value
  // value  := digit { digit }          => Val

  def expr1: P[Expr] =
    term1.chainl1(lassoc)

  def lassoc: P[Op] =
    infix("*")(l => r => Mul(l, r)) | infix("+")(l => r => Add(l, r))

  def term1: P[Expr] =
    (for { _ <- P.reserved("(") ; a <- expr1 ; _ <- P.reserved(")") } yield a) | value

  def value: P[Expr] =
    for { r <- P.digit.oneOrMore } yield Val(r.mkString.toLong)
  
  def parse1(line: String): Expr = 
    P.run(expr1)(line)


  val start1 = System.currentTimeMillis
  println(s"Answer part 1: ${input.map(parse1).map(interpret).sum} [${System.currentTimeMillis - start1}ms]")


  // expr2   := term2 -> { '*' term2 }   => Mul
  // term2   := leaf2 -> { '+' leaf2 }   => Add
  // leaf2   := '('' expr2 ')' | value
  // value   := digit { digit }          => Val

  def expr2: P[Expr] =
    term2.chainl1(mulop)

  def mulop: P[Op] =
    infix("*")(l => r => Mul(l, r))
  
  def term2: P[Expr] =
    leaf2.chainl1(addop)
  
  def addop: P[Op] =
    infix("+")(l => r => Add(l, r))

  def leaf2: P[Expr] =
    (for { _ <- P.reserved("(") ; a <- expr2 ; _ <- P.reserved(")") } yield a) | value
  
  def parse2(line: String): Expr =
    P.run(expr2)(line)

  val start2  = System.currentTimeMillis
  println(s"Answer part 2: ${input.map(parse2).map(interpret).sum} [${System.currentTimeMillis - start2}ms]")
}
