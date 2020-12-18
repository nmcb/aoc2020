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
    P.reserved(op) |~| P.unit(f)

  // value     := digit { digit }        -> Val
  // leaf1     := '('' expr1 ')' | value
  // operation := '*' expr1 | '+' expr1  -> Mul | Add
  // expr1     := leaf { operation1 }

  def value: P[Expr] =
    for { r <- P.digit.oneOrMore } yield Val(r.mkString.toLong)

  def leaf1: P[Expr] =
    (for { _ <- P.reserved("(") ; a <- expr1 ; _ <- P.reserved(")") } yield a) |!| value

  def operation1: P[Op] =
    infix("*")(l => r => Mul(l, r)) |!| infix("+")(l => r => Add(l, r))

  def expr1: P[Expr] =
    leaf1.chainl1(operation1)

  def parse1(line: String): Expr =
    P.run(expr1)(line)


  val start1 = System.currentTimeMillis
  println(s"Answer part 1: ${input.map(parse1).map(interpret).sum} [${System.currentTimeMillis - start1}ms]")


  // value   := digit { digit }       -> Val
  // parens2 := '('' expr2 ')'
  // leaf2   := parens2 | value
  // term    := leaf2 { '+' leaf2 }   -> Add
  // expr    := term { '*' term }     -> Mul

  def leaf2: P[Expr] =
    (for { _ <- P.reserved("(") ; a <- expr2 ; _ <- P.reserved(")") } yield a) |!| value

  def mulop: P[Op] =
    infix("*")(l => r => Mul(l, r))
  
  def addop: P[Op] =
    infix("+")(l => r => Add(l, r))

  def term: P[Expr] =
    leaf2.chainl1(addop)

  def expr2: P[Expr] =
    term.chainl1(mulop)

  def parse2(line: String): Expr =
    P.run(expr2)(line)

  val start2  = System.currentTimeMillis
  println(s"Answer part 2: ${input.map(parse2).map(interpret).sum} [${System.currentTimeMillis - start2}ms]")
}
