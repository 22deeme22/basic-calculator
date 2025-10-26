package calculator
package basic

import cask.endpoints.get
import ujson.Num

class BasicEvaluator extends Evaluator[BasicExpr, BasicEvaluator.Result]:
  import BasicExpr.*
  import BasicEvaluator.*
  import BasicEvaluator.Result.*

  /** Evaluate an expression to its value. */
  def evaluate(e: BasicExpr): Result =
    e match
      case Number(v) => Ok(v)
      case Add(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1+v2)
        case _ => DivByZero
      case Minus(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1-v2)
        case _ => DivByZero
      case Mul(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1*v2)
        case _ => DivByZero
      case Div(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(e1), Ok(0)) => DivByZero
        case (Ok(v1), Ok(v2)) => Ok(v1/v2)
        case _ => DivByZero
      case Neg(e) => evaluate(e) match
        case Ok(v) => Ok(-v)
        case _ => DivByZero
      

object BasicEvaluator:
  enum Result:
    case Ok(v: Double)
    case DivByZero

    def get: Double = this match
      case Ok(v)     => v
      case DivByZero => throw new RuntimeException(s"division by zero")
