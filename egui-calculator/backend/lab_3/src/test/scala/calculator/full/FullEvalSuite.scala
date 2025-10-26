package calculator
package full

import calculator.basic.BasicEvalSuite
import cs214.full.*
import scala.util.{Success, Failure}

class FullEvalSuite extends BasicEvalSuite:
  import FullEvaluator.*
  import FullEvaluator.Result.*

  protected class UndefinedVarException(m: String = "") extends Exception(m)

  /** Dummy context with ((x, 1), (y, 2), (z, 3)) */
  protected val xyzContext = Context.cons("x", 1, Context.cons("y", 2, Context.cons("z", 3, Context.empty)))

  override protected def name: String = "Full"
  override protected def evaluate(source: String): Double =
    // Uses xyz context
    FullDriver(xyzContext).evaluate(source).get match
      case Ok(v)              => v
      case DivByZero          => throw new DivideByZeroException()
      case UndefinedVar(name) => throw new UndefinedVarException()

  override protected def okTests: List[(String, Double)] = super.okTests ++ List(
    "x+2" -> 3,
    "y-1" -> 1,
    "z*2" -> 6,
    "-x" -> -1,
    "y/2" -> 1,
    "x+y-z" -> 0,
    "x*2+y" -> 4
  )
  override protected def divByZeroTests: List[String] = super.divByZeroTests ++ List(
    "x/0",
    "y/(-1+1)",
    "z/(1-1)"
  )
  protected def undefinedVarTests: List[String] = List(
    "a",
    "2+c",
    "x+a",
    "y*b",
    "z-xy"
  )

  for i <- undefinedVarTests do
    test(s"${name}Evaluator: ${name}Driver.evaluate(\"$i\").get should result in an UndefinedVar result (0.8pt)"):
      intercept[UndefinedVarException]:
        evaluate(i)
