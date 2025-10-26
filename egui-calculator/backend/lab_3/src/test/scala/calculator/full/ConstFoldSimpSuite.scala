package calculator
package full

import calculator.full.FullExpr.*
import cs214.full.*

final class ConstFoldSimpSuite extends SimpSuite:
  def name = "ConstFold"
  def driver = ConstFoldDriver(FullEvaluator.Context.empty)
  def tests: List[(String, FullExpr)] = ConstFoldSimpSuite.sharedTests ++ ConstFoldSimpSuite.incompleteTests

object ConstFoldSimpSuite:
  import SimpSuite.*

  // Tests that yields same result on full simplifier
  val sharedTests: List[(String, FullExpr)] =
    if SimpSuite.enableDebugTests then
      List(
        // No simplification
        "1" -> 1.asNum,
        "a" -> "a".asVar,
        "a+a" -> ("a".asVar + "a".asVar),
        "a+b" -> ("a".asVar + "b".asVar),
        "a+1+2" -> ("a".asVar + 1.asNum + 2.asNum), // Tree is ((a+1)+2) so no simplification
        "a+1+2+b" -> ("a".asVar + 1.asNum + 2.asNum + "b".asVar), // Same, ((a+1)+(2+b))
        "a-b" -> ("a".asVar - "b".asVar),
        "a-1-2" -> ("a".asVar - 1.asNum - 2.asNum),
        "a-1-2-b" -> ("a".asVar - 1.asNum - 2.asNum - "b".asVar),
        "a*a" -> ("a".asVar * "a".asVar),
        "a*b" -> ("a".asVar * "b".asVar),
        "a/0" -> ("a".asVar / 0.asNum),
        "0/a" -> (0.asNum / "a".asVar),
        "a/b" -> ("a".asVar / "b".asVar),
        // Add
        "1+1" -> 2.asNum,
        "0+2" -> 2.asNum,
        "2+0" -> 2.asNum,
        "(1+1)+(2+2)" -> 6.asNum,
        "(100+200)+(300+400)" -> 1000.asNum,
        "(2-1)+(1-2)" -> 0.asNum,
        "(2*2)+(1*2)" -> 6.asNum,
        "(6/3)+(6/2)" -> 5.asNum,
        "6+(-6)" -> 0.asNum,
        "a+(1+2)" -> ("a".asVar + 3.asNum),
        "1+2+a" -> (3.asNum + "a".asVar),
        // Minus
        "1-1" -> 0.asNum,
        "0-2" -> -2.asNum,
        "2-0" -> 2.asNum,
        "(2-1)-(2-4)" -> 3.asNum,
        "(100-200)-(300-400)" -> 0.asNum,
        "(2+1)-(1+2)" -> 0.asNum,
        "(2*2)-(1*2)" -> 2.asNum,
        "(6/3)-(6/2)" -> -1.asNum,
        "6-(-6)" -> 12.asNum,
        "a-(1-2)" -> ("a".asVar - -1.asNum),
        "1-2-a" -> (-1.asNum - "a".asVar),
        // Mul
        "1*2" -> 2.asNum,
        "0*2" -> 0.asNum,
        "2*0" -> 0.asNum,
        "(2*1)*(2*2)" -> 8.asNum,
        "(100*100)*(20*2)" -> 400000.asNum,
        "(2+1)*(1+2)" -> 9.asNum,
        "(2-3)*(1-2)" -> 1.asNum,
        "(6/3)*(6/2)" -> 6.asNum,
        "6*(-6)" -> -36.asNum,
        "a*(2*3)" -> ("a".asVar * 6.asNum),
        "2*3*a" -> (6.asNum * "a".asVar),
        // Neg
        "-0" -> 0.asNum,
        "-1" -> -1.asNum,
        "-500" -> -500.asNum,
        "-(-6)" -> 6.asNum,
        "-(2+3)" -> -5.asNum,
        "-(4-6)" -> 2.asNum,
        "-(-4*5)" -> 20.asNum,
        "-(-20/-2)" -> -10.asNum,
        // Div
        "1/2" -> 0.5.asNum,
        "0/2" -> 0.asNum,
        "(2/1)/(2/2)" -> 2.asNum,
        "(2000/10)/(50/10)" -> 40.asNum,
        "(2+1)/(1+2)" -> 1.asNum,
        "(2-3)/(1-2)" -> 1.asNum,
        "(6/3)/(6/2)" -> (2d / 3d).asNum,
        "6/(-6)" -> -1.asNum,
        "a/(3/2)" -> ("a".asVar / (3d / 2d).asNum),
        "6/3/a" -> (2.asNum / "a".asVar)
      )
    else
      Nil

  // Tests that are further simplified with full simplifier
  private val incompleteTests: List[(String, FullExpr)] =
    if SimpSuite.enableDebugTests then
      List(
        // No simplification here
        "a+0" -> ("a".asVar + 0.asNum),
        "0+a" -> (0.asNum + "a".asVar),
        "a-0" -> ("a".asVar - 0.asNum),
        "0-a" -> (0.asNum - "a".asVar),
        "a-a" -> ("a".asVar - "a".asVar),
        "a*0" -> ("a".asVar * 0.asNum),
        "0*a" -> (0.asNum * "a".asVar),
        "1*a" -> (1.asNum * "a".asVar),
        "a*1" -> ("a".asVar * 1.asNum),
        "-(-a)" -> "a".asVar.neg.neg,
        "a/1" -> ("a".asVar / 1.asNum),
        "a/a" -> ("a".asVar / "a".asVar),
        "a*1*2" -> ("a".asVar * 1.asNum * 2.asNum),
        "a/1/2" -> ("a".asVar / 1.asNum / 2.asNum),
        "a/1/2/b" -> ("a".asVar / 1.asNum / 2.asNum / "b".asVar),
        "a*1*2*b" -> ("a".asVar * 1.asNum * 2.asNum * "b".asVar)
      )
    else
      Nil
