package cs214

import calculator.*
import scala.util.Try

trait Driver[E <: Expr, R]:
  def evaluator: Evaluator[E, R]
  def parser(source: List[Token]): Parser[E]

  def parse(source: String): Try[E] = Try:
    val tokens = Tokenizer.tokenize(source).toTry.get
    parser(tokens).parse.toTry.get

  def evaluate(source: String): Try[R] = Try:
    val expr = parse(source).get
    evaluator.evaluate(expr)

  def evaluate(e: E): Try[R] = Try:
    evaluator.evaluate(e)
