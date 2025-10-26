package calculator
package full

import ujson.Num

/** Folds constant sub-expressions in values. */
object ConstFoldSimplifier extends Simplifier[FullExpr]:
  import FullExpr.*

  def simplify(e: FullExpr): FullExpr =
    e match
      case Number(value) => Number(value)
      case Add(e1, e2) => (simplify(e1), simplify(e2)) match
        case (Number(v1), Number(v2)) => Number(v1+v2)
        case (x,y) => Add(x,y)
      case Minus(e1, e2) => (simplify(e1), simplify(e2)) match
        case (Number(v1), Number(v2)) => Number(v1 - v2)
        case (x,y) => Minus(x, y)
      case Mul(e1, e2) => (simplify(e1), simplify(e2)) match
        case (Number(v1), Number(v2)) => Number(v1*v2)
        case (x, y) => Mul(x, y)
      case Div(e1, e2) => (simplify(e1), simplify(e2)) match
        case (Number(v1), Number(v2)) => Number(v1/v2)
        case (x, y) => Div(x, y)
      case Neg(e) =>  simplify(e) match
        case Number(v) => Number(-v)
        case x => Neg(x)
      case Var(name) => Var(name)
      

      
    
