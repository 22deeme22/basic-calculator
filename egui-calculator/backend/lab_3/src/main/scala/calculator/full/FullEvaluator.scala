package calculator.full

import scala.util.{Try, Success, Failure}
import scala.annotation.threadUnsafe
import calculator.Evaluator

object FullEvaluator:
  /** Result of evaluation. */
  enum Result:
    case Ok(v: Double)
    case DivByZero
    case UndefinedVar(name: String)

    def get: Double = this match
      case Ok(v)              => v
      case DivByZero          => throw new RuntimeException("division by zero")
      case UndefinedVar(name) => throw new RuntimeException(s"undefined variable: $name")

  // Define your own context here
  enum MyContext: 
      case empty
      case variable(name: String, value: Double, tail: MyContext)
      def lookup(name: String): Result =
        this match
          case MyContext.empty => Result.UndefinedVar(name)
          case MyContext.variable(n, v, tail) =>
            if n == name then Result.Ok(v)
            else tail.lookup(name)

  type Context =
    MyContext

  object Context:
    def empty: Context =
      MyContext.empty

    def cons(name: String, value: Double, tail: Context) =
      MyContext.variable(name, value, tail) 

    def fromList(xs: List[(String, Double)]): Context =
      xs match
        case Nil           => empty
        case (n, v) :: rem => cons(n, v, fromList(rem))

class FullEvaluator(ctx: FullEvaluator.Context) extends Evaluator[FullExpr, FullEvaluator.Result]:
  import FullEvaluator.*
  import FullExpr.*
  import Result.*

  /** Evaluate an expression to its value. */
  def evaluate(e: FullExpr): Result =
    e match
      case Number(v) => Ok(v)
      case Add(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1+v2)
        case (DivByZero, _) | (_, DivByZero) => DivByZero
        case (UndefinedVar(n), _) => UndefinedVar(n)
        case (_, UndefinedVar(m)) => UndefinedVar(m)
      case Minus(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1-v2)
        case (DivByZero, _) | (_, DivByZero) => DivByZero
        case (UndefinedVar(n), _) => UndefinedVar(n)
        case (_, UndefinedVar(m)) => UndefinedVar(m)
      case Mul(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1*v2)
        case (DivByZero, _) | (_, DivByZero) => DivByZero
        case (UndefinedVar(n), _)  => UndefinedVar(n)
        case (_, UndefinedVar(m)) => UndefinedVar(m)
      case Div(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(e1), Ok(0)) => DivByZero
        case (Ok(v1), Ok(v2)) => Ok(v1/v2)
        case (DivByZero, _) | (_, DivByZero) => DivByZero
        case (UndefinedVar(n), _) => UndefinedVar(n)
        case (_, UndefinedVar(m)) => UndefinedVar(m)
      case Neg(e) => evaluate(e) match
        case Ok(v) => Ok(-v)
        case (DivByZero) => DivByZero
        case (UndefinedVar(n)) => UndefinedVar(n)
      case Var(name) => ctx.lookup(name)
            
           
        
      

        
    

