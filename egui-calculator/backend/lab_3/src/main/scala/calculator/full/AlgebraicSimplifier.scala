package calculator
package full

import ujson.Num

/** Simplify expressions based on the listed algebraic rules
  * {{{
  * 1. 0 + e = e + 0 = e
  * 2. 0 - e = -e
  * 3. e - 0 = e
  * 4. 0 * e = e * 0 = 0
  * 5. 1 * e = e * 1 = e
  * 6. e / 1 = e
  * 7. e - e = 0
  * 8. -(-e) = e
  * }}}
  */
object AlgebraicSimplifier extends Simplifier[FullExpr]:
  import FullExpr.*

  def simplify(e: FullExpr): FullExpr =
    e match
      case Number(value) => Number(value)
      case Add(e1, e2) => (simplify(e1), simplify(e2)) match
        case (Number(0), x) => x
        case (x, Number(0)) => x
        case (x,y) => Add(x,y)
      case Minus(e1, e2) => (simplify(e1), simplify(e2)) match
        case (x1, x2) if x1 == x2 => Number(0)
        case (Number(0), x) => Neg(x)
        case (x, Number(0)) => x
        case (x,y) => Minus(x, y)
      case Mul(e1, e2) => (simplify(e1), simplify(e2)) match
        case (Number(1), x) => x
        case (x, Number(1)) => x
        case (x, Number(0)) => Number(0)
        case (Number(0), x) => Number(0)
        case (x, y) => Mul(x, y)
      case Div(e1, e2) => (simplify(e1), simplify(e2)) match
        case (x, Number(1)) => x
        case (x, y) => Div(x, y)
      case Neg(e) =>  simplify(e) match
        case Neg(e) => e
        case x => Neg(x)
      case Var(name) => Var(name)
