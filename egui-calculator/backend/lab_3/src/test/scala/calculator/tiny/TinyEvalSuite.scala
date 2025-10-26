package calculator
package tiny

import cs214.tiny.*

open class TinyEvalSuite extends EvalSuite:
  override protected def name: String = "Tiny"
  override protected def evaluate(source: String): Double =
    TinyDriver.evaluate(source).get
  override protected def okTests: List[(String, Double)] = List(
    "0" -> 0,
    "100" -> 100,
    "0+1" -> 1,
    "10+20" -> 30,
    "2-1" -> 1,
    "1-2" -> -1,
    "1-(1-2)" -> 2,
    "0*1" -> 0,
    "1*12" -> 12,
    "4*5" -> 20,
    "-1" -> -1,
    "1+2-3" -> 0,
    "-1+1" -> 0,
    "-1-(-1)" -> 0,
    "1-2-3+2+3" -> 1,
    "1*2+3*4-10" -> 4
  )
