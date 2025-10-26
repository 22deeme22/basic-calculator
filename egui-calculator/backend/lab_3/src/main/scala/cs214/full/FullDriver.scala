package cs214
package full

import calculator.*
import calculator.full.*
import scala.util.Try

private case class Block(defs: List[(String, FullExpr)], expr: FullExpr)

sealed trait FullDriverT extends Driver[FullExpr, FullEvaluator.Result]:
  def simp: Simplifier[FullExpr]
  def ctx: FullEvaluator.Context

  def evaluator: Evaluator[FullExpr, FullEvaluator.Result] = FullEvaluator(ctx)
  def parser(source: List[Token]): Parser[FullExpr] = FullParser(source)

  def simplify(source: String): Try[FullExpr] = Try:
    val expr = parse(source).get
    simp.simplify(expr)

  def simplify(expr: FullExpr): Try[FullExpr] = Try:
    simp.simplify(expr)

  def parseDef(source: String): Try[(String, FullExpr) | FullExpr] = Try:
    val tokens = Tokenizer.tokenize(source).toTry.get
    val result = FullParser(tokens).parseDef
    result.toTry.get

  def parseAssign(source: String): Try[(String, FullExpr)] = Try:
    val tokens = Tokenizer.tokenize(source).toTry.get
    val result = FullParser(tokens).parseAssign
    result.toTry.get

  def parseBlock(source: String): Try[Block] = Try:
    def recur(lines: List[String], acc: List[(String, FullExpr)]): Block =
      lines match
        case x1 :: xs =>
          xs match
            case Nil =>
              val e = parse(x1).get
              Block(acc.reverse, e)
            case xs =>
              val d = parseAssign(x1).get
              recur(xs, d :: acc)
        case Nil => throw RuntimeException("empty input")
    val lines = source.linesIterator.toList
    recur(lines, Nil)

class ConstFoldDriver(val ctx: FullEvaluator.Context) extends FullDriverT:
  def simp: Simplifier[FullExpr] = ConstFoldSimplifier

class AlgebraicDriver(val ctx: FullEvaluator.Context) extends FullDriverT:
  def simp: Simplifier[FullExpr] = AlgebraicSimplifier

class FullDriver(val ctx: FullEvaluator.Context) extends FullDriverT:
  def simp: Simplifier[FullExpr] = FullSimplifier

object FullDriver:
  import Printers.show
  import FullEvaluator.Result as Result

  case class EvaluatedBlock(defs: List[(String, Double)], result: Result)

  def evaluateBlock(block: Block): Try[EvaluatedBlock] = Try:
    var curCtx = FullEvaluator.Context.empty
    var curAcc: List[(String, Double)] = Nil
    block.defs.foreach: (name, expr) =>
      val res = FullDriver(curCtx).evaluate(expr).get match
        case Result.Ok(v)     => v
        case Result.DivByZero => throw RuntimeException(s"error when evaluating ${expr.show}: division by zero")
        case Result.UndefinedVar(name) =>
          throw RuntimeException(s"error when evaluating ${expr.show}: undefined var $name")
      curCtx = FullEvaluator.Context.cons(name, res, curCtx)
      curAcc = (name -> res) :: curAcc
    EvaluatedBlock(curAcc.reverse, FullDriver(curCtx).evaluate(block.expr).get)
