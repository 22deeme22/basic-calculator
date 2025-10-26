package calculator
package tiny

object TinyPrinter:
  import TinyExpr.*

  /** Re-implement the `toString` function of TinyExpr. */
  def show(e: TinyExpr): String =
    e match
      case Number(value) => f"Number($value)"
      case Add(e1, e2) => f"Add(${show(e1)},${show(e2)})"
      case Minus(e1, e2) => f"Minus(${show(e1)},${show(e2)})"
      case Mul(e1, e2)   => f"Mul(${show(e1)},${show(e2)})"
      case Neg(e)        => f"Neg(${show(e)})"    

  /** Print the expression in Polish notation. */
  def toPolish(e: TinyExpr): String =
    e match
      case Number(value) => f"$value"
      case Add(e1, e2) => f"+ ${toPolish(e1)} ${toPolish(e2)}"
      case Minus(e1, e2) => f"- ${toPolish(e1)} ${toPolish(e2)}"
      case Mul(e1, e2) => f"* ${toPolish(e1)} ${toPolish(e2)}"
      case Neg(e) => f"-- ${toPolish(e)}"



    
