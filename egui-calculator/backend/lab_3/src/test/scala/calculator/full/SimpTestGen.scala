package calculator
package full

import util.Random
import calculator.full.FullExpr
import FullExpr.*

class SeedGen(using rnd: Random):
  def generateList(maxWidth: Int = 10, varName: String = "x"): List[(Double, String)] =
    def recur(acc: List[(Double, String)]): List[(Double, String)] =
      if acc.length >= maxWidth then acc
      else
        rnd.between(0, 2) match
          case 0 => acc
          case 1 =>
            val name = s"$varName${acc.length}"
            val acc1 = (rnd.between(-10.0, 10.0), name) :: acc
            recur(acc1)
    recur(Nil)

  def compile(xs: List[(Double, String)]): FullExpr =
    def recur(head: (Double, String), rem: List[(Double, String)]): FullExpr = rem match
      case Nil           => Mul(Number(head._1), Var(head._2))
      case head1 :: rem1 => Add(Mul(Number(head._1), Var(head._2)), recur(head1, rem1))
    xs match
      case head :: rem => recur(head, rem)
      case Nil         => Number(0.0)

  def generate(maxWidth: Int = 10, varName: String = "x"): FullExpr = compile(generateList(maxWidth, varName))

object SeedGen:
  def generate(maxWidth: Int = 10, varName: String = "x")(using rnd: Random): FullExpr =
    val gen = SeedGen()
    gen.generate(maxWidth, varName)

class SimpTestGen(using rnd: Random):
  def options(e: FullExpr): List[FullExpr] =
    val univ = List(
      Add(Number(0), e),
      Add(e, Number(0)),
      Minus(e, Number(0)),
      Mul(Number(1), e),
      Mul(e, Number(1)),
      Div(e, Number(1))
    )
    e match
      case Number(0) =>
        val e = SeedGen.generate(maxWidth = 10, varName = "y")
        Minus(e, e) :: univ
      case Neg(e) => Minus(Number(0), e) :: univ
      case _      => univ

  def expand(e: FullExpr): Option[FullExpr] =
    def expandThis = options(e) match
      case Nil => None
      case xs =>
        val i = rnd.between(0, xs.length)
        Some(xs(i))

    def randomTryExpand(e1: FullExpr, e2: FullExpr, reconstruct: (FullExpr, FullExpr) => FullExpr): Option[FullExpr] =
      rnd.between(0, 2) match
        case 0 => expand(e1) match
            case None     => expand(e2).map(e2 => reconstruct(e1, e2))
            case Some(e1) => Some(reconstruct(e1, e2))
        case 1 => expand(e2) match
            case None     => expand(e1).map(e1 => reconstruct(e1, e2))
            case Some(e2) => Some(reconstruct(e1, e2))

    def expandChildren = e match
      case Number(value) => None
      case Add(e1, e2)   => randomTryExpand(e1, e2, Add(_, _))
      case Minus(e1, e2) => randomTryExpand(e1, e2, Minus(_, _))
      case Mul(e1, e2)   => randomTryExpand(e1, e2, Mul(_, _))
      case Div(e1, e2)   => randomTryExpand(e1, e2, Div(_, _))
      case Neg(e)        => expand(e).map(Neg(_))
      case Var(name)     => None

    rnd.between(0, 2) match
      case 0 => expandThis.orElse(expandChildren)
      case 1 => expandChildren

  def maybeExpand(e: FullExpr): FullExpr = expand(e).getOrElse(e)

  def generate(numExpansions: Int): (FullExpr, FullExpr) =
    val seed = SeedGen.generate()
    var result = seed
    (0 until numExpansions).foreach: _ =>
      result = maybeExpand(result)
    (seed, result)

object SimpTestGen:
  def generate(numExpansions: Int)(using Random): (FullExpr, FullExpr) =
    val gen = SimpTestGen()
    gen.generate(numExpansions)
