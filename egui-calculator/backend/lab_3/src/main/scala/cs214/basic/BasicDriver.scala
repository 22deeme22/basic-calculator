package cs214
package basic

import calculator.basic.*
import calculator.basic.BasicEvaluator.Result
import calculator.Evaluator

object BasicDriver extends Driver[BasicExpr, BasicEvaluator.Result]:
  def evaluator = BasicEvaluator()
  def parser(source: List[Token]) = BasicParser(source)
