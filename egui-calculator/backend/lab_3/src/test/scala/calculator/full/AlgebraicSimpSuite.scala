package calculator
package full

import calculator.full.FullExpr.*
import cs214.full.*

final class AlgebraicSimpSuite extends SimpSuite:
  def name = "Algebraic"
  def driver = AlgebraicDriver(FullEvaluator.Context.empty)
  def tests: List[(String, FullExpr)] = AlgebraicSimpSuite.sharedTests ++ AlgebraicSimpSuite.incompleteTests

object AlgebraicSimpSuite:
  import SimpSuite.*

  // Easier debug shared tests for regular track. Can be disabled with SimpSuite.enableDebugTests
  private val debugSharedTests: List[(String, FullExpr)] = List(
    // No simplification
    "a" -> "a".asVar,
    "a+1" -> ("a".asVar + 1.asNum),
    "1+a" -> (1.asNum + "a".asVar),
    "a-1" -> ("a".asVar - 1.asNum),
    "1-a" -> (1.asNum - "a".asVar),
    "a*2" -> ("a".asVar * 2.asNum),
    "2*a" -> (2.asNum * "a".asVar),
    "-a" -> "a".asVar.neg,
    "-(a+1)" -> ("a".asVar + 1.asNum).neg,
    "a/2" -> ("a".asVar / 2.asNum),
    "2/a" -> (2.asNum / "a".asVar),
    "0/a" -> (0.asNum / "a".asVar),
    "a/0" -> ("a".asVar / 0.asNum),
    "a+b" -> ("a".asVar + "b".asVar),
    "a-b" -> ("a".asVar - "b".asVar),
    "a*a" -> ("a".asVar * "a".asVar),
    "-(a+b)" -> ("a".asVar + "b".asVar).neg,
    "a/b" -> ("a".asVar / "b".asVar),
    "a/(a+1)" -> ("a".asVar / ("a".asVar + 1.asNum)),
    "a+(-b)" -> ("a".asVar + "b".asVar.neg),
    "a+(-a)" -> ("a".asVar + "a".asVar.neg),
    // 1. (v+0), (0+v)
    "a+0" -> "a".asVar,
    "(a*b)+0" -> ("a".asVar * "b".asVar),
    "0+a" -> "a".asVar,
    "0+a/b" -> ("a".asVar / "b".asVar),
    // 2. (v-0)
    "a-0" -> "a".asVar,
    "(a*b)-0" -> ("a".asVar * "b".asVar),
    // 3. (0-v)
    "0-a" -> "a".asVar.neg,
    "0-(a/b)" -> ("a".asVar / "b".asVar).neg,
    // 4. (v*0), (0*v)
    "a*0" -> 0.asNum,
    "(a*b)*0" -> 0.asNum,
    "0*a" -> 0.asNum,
    "0*(a/b)" -> 0.asNum,
    // 5. (v*1), (1*v)
    "a*1" -> "a".asVar,
    "(a*b)*1" -> ("a".asVar * "b".asVar),
    "1*a" -> "a".asVar,
    "1*(a/b)" -> ("a".asVar / "b".asVar),
    // 6. (v/1)
    "a/1" -> "a".asVar,
    "(a+b)/1" -> ("a".asVar + "b".asVar),
    "a/b/1" -> ("a".asVar / "b".asVar),
    // 7. (v-v)
    "a-a" -> 0.asNum,
    "(a-b)-(a-b)" -> 0.asNum,
    "(a/a)-(a/a)" -> 0.asNum,
    // 8. -(-v)
    "-(-a)" -> "a".asVar,
    "-(-(a*b))" -> ("a".asVar * "b".asVar)
  )

  // Tests that yields same result on full simplifier
  val sharedTests: List[(String, FullExpr)] =
    if SimpSuite.enableDebugTests then debugSharedTests
    else
      Nil ++
        List(
          // Mix
          "(a-a+1)*a*a-a*a" -> 0.asNum,
          "(0+a-a)*b" -> 0.asNum,
          "1*a-0" -> "a".asVar,
          "a*a-0*(a+b+c/d)" -> ("a".asVar * "a".asVar),
          "a*a*a*1" -> ("a".asVar * "a".asVar * "a".asVar)
        )

  // Easier debug incomplete tests for regular track. Can be disabled with SimpSuite.enableDebugTests
  private val debugIncompleteTests: List[(String, FullExpr)] = List(
    // No simplification
    "1+2" -> (1.asNum + 2.asNum),
    "1-2" -> (1.asNum - 2.asNum),
    "2*2" -> (2.asNum * 2.asNum),
    "-1" -> 1.asNum.neg,
    "1/2" -> (1.asNum / 2.asNum)
  )
  // Tests that are further simplified with full simplifier
  private val incompleteTests: List[(String, FullExpr)] =
    if SimpSuite.enableDebugTests then debugIncompleteTests
    else
      Nil ++
        List(
          // Mix
          "a*(1+2)" -> ("a".asVar * (1.asNum + 2.asNum)),
          "a*(1+2)+(b-b)" -> ("a".asVar * (1.asNum + 2.asNum)),
          "a*(1+2)*(b*(c-c)+1)+(b-b)" -> ("a".asVar * (1.asNum + 2.asNum))
        )
