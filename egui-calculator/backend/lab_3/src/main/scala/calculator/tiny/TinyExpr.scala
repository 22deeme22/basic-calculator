package calculator
package tiny

enum TinyExpr extends Expr:
  case Number(value: Double)
  case Add(e1: TinyExpr, e2: TinyExpr)
  case Minus(e1: TinyExpr, e2: TinyExpr)
  case Mul(e1: TinyExpr, e2: TinyExpr)
  case Neg(e: TinyExpr)

  import full.FullExpr
  def embed: FullExpr = this match
    case Number(value) => FullExpr.Number(value)
    case Add(e1, e2)   => FullExpr.Add(e1.embed, e2.embed)
    case Minus(e1, e2) => FullExpr.Minus(e1.embed, e2.embed)
    case Mul(e1, e2)   => FullExpr.Mul(e1.embed, e2.embed)
    case Neg(e)        => FullExpr.Neg(e.embed)

