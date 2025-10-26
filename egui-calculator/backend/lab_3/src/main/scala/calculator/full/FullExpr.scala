package calculator
package full

enum FullExpr extends Expr:
  case Number(value: Double)
  case Add(e1: FullExpr, e2: FullExpr)
  case Minus(e1: FullExpr, e2: FullExpr)
  case Mul(e1: FullExpr, e2: FullExpr)
  case Div(e1: FullExpr, e2: FullExpr)
  case Neg(e: FullExpr)
  case Var(name: String) /* new case */
