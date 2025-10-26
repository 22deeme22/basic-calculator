package calculator
package full

/** Fully (const folding and algebraic) simplify expressions */
object FullSimplifier extends Simplifier[FullExpr]:
  import FullExpr.*

  def simplify(e: FullExpr): FullExpr =
    e match
      case Number(value) => Number(value)
      case Add(e1, e2) => (simplify(e1), simplify(e2)) match
        case (Number(0), x) => x
        case (x, Number(0)) => x
        case (Number(v1), Number(v2)) => Number(v1+v2)
        case (x,y) => Add(x,y)
      case Minus(e1, e2) => (simplify(e1), simplify(e2)) match
        case (x1, x2) if x1 == x2 => Number(0)
        case (x, Number(0)) => x
        case (Number(v1), Number(v2)) => Number(v1 - v2)
        case (Number(0), x) => Neg(x)
        case (x,y) => Minus(x, y)
      case Mul(e1, e2) => (simplify(e1), simplify(e2)) match
        case (Number(1), x) => x
        case (x, Number(1)) => x
        case (x, Number(0)) => Number(0)
        case (Number(0), x) => Number(0)
        case (Number(v1), Number(v2)) => Number(v1*v2)
        case (x, y) => Mul(x, y)
      case Div(e1, e2) => (simplify(e1), simplify(e2)) match
        case (x, Number(1)) => x
        case (Number(v1), Number(v2)) => Number(v1/v2)
        case (x, y) => Div(x, y)
      case Neg(e) =>  simplify(e) match
        case Neg(e) => e
        case Number(v) => Number(-v)
        case x => Neg(x)
      case Var(name) => Var(name)
