package calculator
package basic

enum BasicExpr extends Expr:
  case Number(value: Double)
  case Add(e1: BasicExpr, e2: BasicExpr)
  case Minus(e1: BasicExpr, e2: BasicExpr)
  case Mul(e1: BasicExpr, e2: BasicExpr)
  case Div(e1: BasicExpr, e2: BasicExpr) /* new case */
  case Neg(e: BasicExpr)
