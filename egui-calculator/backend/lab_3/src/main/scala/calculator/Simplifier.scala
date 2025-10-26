package calculator

trait Simplifier[E <: Expr]:
  def simplify(e: E): E
