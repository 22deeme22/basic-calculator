package calculator
package tiny

class TinyEvaluator extends Evaluator[TinyExpr, Double]:
  import TinyExpr.*

  /** Evaluate an expression to its value. */
  def evaluate(e: TinyExpr): Double =
    e match
      case Number(value) => value
      case Add(e1, e2) => evaluate(e1) + evaluate(e2)
      case Minus(e1, e2) => evaluate(e1) - evaluate(e2)
      case Mul(e1, e2) => evaluate(e1) * evaluate(e2)
      case Neg(e) => 0 - evaluate(e)
    
