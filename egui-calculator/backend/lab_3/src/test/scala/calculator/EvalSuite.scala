package calculator

trait EvalSuite extends munit.FunSuite:
  protected val DELTA = 0.00001

  protected def name: String
  protected def evaluate(source: String): Double
  protected def okTests: List[(String, Double)] // def because can't access to super.okTest if val

  for (i, o) <- okTests do
    test(f"${name}Evaluator: ${name}Driver.evaluate(\"$i\").get should be $o (1pt)"):
      assertEqualsDouble(evaluate(i), o, DELTA)
