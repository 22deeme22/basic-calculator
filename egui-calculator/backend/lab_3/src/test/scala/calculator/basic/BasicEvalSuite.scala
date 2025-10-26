package calculator
package basic

import cs214.basic.*
import calculator.tiny.TinyEvalSuite
import scala.util.{Success, Failure}

open class BasicEvalSuite extends TinyEvalSuite:
  import BasicEvaluator.*

  protected class DivideByZeroException(m: String = "") extends Exception(m)

  override protected def name: String = "Basic"
  override protected def evaluate(source: String): Double = BasicDriver.evaluate(source).get match
    case Result.Ok(v)     => v
    case Result.DivByZero => throw new DivideByZeroException()
  override protected def okTests: List[(String, Double)] = super.okTests ++ List(
    "4/2" -> 2,
    "2/1" -> 2,
    "0/50" -> 0,
    "1/10" -> 0.1,
    "1/0.1" -> 10,
    "-1/2" -> -0.5,
    "-1/(1+1)" -> -0.5,
    "1/(1/2)" -> 2,
    "-(-2*2)/(1/5)-19" -> 1,
    "2*(3+4/2)" -> 10
  )
  protected def divByZeroTests: List[String] = List(
    "0/0",
    "1/0",
    "1/(1-1)",
    "1/(1/0)",
    "1+3/(2*0)"
  )

  for i <- divByZeroTests do
    test(s"${name}Evaluator: ${name}Driver.evaluate(\"$i\").get should result in a DivByZero result (0.8pt)"):
      intercept[DivideByZeroException]:
        evaluate(i)
