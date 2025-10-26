package calculator

trait Evaluator[E <: Expr, R]:
  def evaluate(e: E): R
