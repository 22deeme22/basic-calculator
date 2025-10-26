package cs214
package tiny

import calculator.tiny.*
import ui.GraphvizPrinter.toGraphviz
import scala.util.Try

object TinyDriver extends Driver[TinyExpr, Double]:
  def evaluator = TinyEvaluator()
  def parser(source: List[Token]) = TinyParser(source)

  def render(source: String): Try[String] = Try:
    val expr = parse(source).get
    expr.toGraphviz
