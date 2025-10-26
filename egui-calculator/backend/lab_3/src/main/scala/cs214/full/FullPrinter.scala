package cs214
package full

import calculator.full.FullExpr

object FullPrinter:
  import FullExpr.*
  def bindingLevel(e: FullExpr): Int =
    e match
      case Number(value) => 4
      case Var(name)     => 4
      case Neg(e)        => 3
      case Mul(e1, e2)   => 2
      case Div(e1, e2)   => 2
      case Add(e1, e2)   => 1
      case Minus(e1, e2) => 1

  def show(e: FullExpr, curLevel: Int = 0): String =
    def recur(e: FullExpr, curLevel: Int): String =
      val level = bindingLevel(e)
      val s = e match
        case Number(value) => value.toString
        case Add(e1, e2)   => s"${recur(e1, level)} + ${recur(e2, level + 1)}"
        case Minus(e1, e2) => s"${recur(e1, level)} - ${recur(e2, level + 1)}"
        case Mul(e1, e2)   => s"${recur(e1, level)} * ${recur(e2, level + 1)}"
        case Div(e1, e2)   => s"${recur(e1, level)} / ${recur(e2, level + 1)}"
        case Neg(e)        => s"-${recur(e, level)}"
        case Var(name)     => name
      if level < curLevel then s"($s)" else s
    recur(e, curLevel = curLevel)

  extension (e: FullExpr)
    def show: String = FullPrinter.show(e, 0)
