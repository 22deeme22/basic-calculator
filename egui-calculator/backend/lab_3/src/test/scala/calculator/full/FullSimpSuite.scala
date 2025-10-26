package calculator
package full

import calculator.full.FullExpr.*
import cs214.full.*
import cs214.full.FullPrinter.*
import scala.util.Random

final class FullSimpSuite extends FullSimpSuiteT:
  def name = "Full"
  def driver = FullDriver(FullEvaluator.Context.empty)

// It is a class and not an object to possibly choose different drivers
trait FullSimpSuiteT() extends SimpSuite:
  import SimpSuite.*

  def tests: List[(String, FullExpr)] = AlgebraicSimpSuite.sharedTests ++ ConstFoldSimpSuite.sharedTests ++ List(
    "((0+a)-(a+0)+1)*(2+3)" -> 5.asNum,
    "((0-a)+(a-0)+1)*(2+3)" -> ("a".asVar.neg + "a".asVar + 1.asNum) * 5.asNum,
    "((0*a)+(a*0)+1)*(2+3)" -> 5.asNum,
    "((1*a)+(a*1))*(2+3)" -> (("a".asVar + "a".asVar) * 5.asNum),
    "((a/1)-a+(a/1))*(2+3)" -> ("a".asVar * 5.asNum),
    "(a-(3-2)*a)*(2+3)" -> 0.asNum,
    "(-a-(-(-a)))*(2+3)" -> (("a".asVar.neg - "a".asVar) * 5.asNum)
  )

  test(
    f"${name}Simplifier: deep recursion tests (no composition of kind (...(algebraic(constfold(e)))) is used) (0.5pt)"
  ):
    def deepCombinationTestCase(depth: Int) =
      (0 to depth).foldLeft("2")((acc, _) => f"($acc/2*a-a+2)")
    val MAX_DEPTH = 125
    assertEquals(driver.simplify(deepCombinationTestCase(MAX_DEPTH)).get, 2.asNum)

  given simpRnd: Random = new Random(42)
  test(f"${name}Simplifier: randomized tests (11.5pt)") {
    (0 until 1000).foreach: idx =>
      val (key, expr) = SimpTestGen.generate(idx + 5)
      val answer = driver.simplify(expr).get
      assertEquals(
        answer,
        key,
        f"${name}Simplifier random tests: ${name}Driver.simplify(\"${expr.show}\") should be ${key.show}, but it is actually ${answer.show}"
      )
  }
