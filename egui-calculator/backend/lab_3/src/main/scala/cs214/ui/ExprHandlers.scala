package cs214.ui

import calculator.full.*
import cs214.tiny.TinyDriver
import cs214.basic.BasicDriver
import cs214.full.FullDriver
import cs214.full.FullPrinter.show
import GraphvizPrinter.*

import util.{Try, Success, Failure}

import upickle.default.*

object ExprHandlers:
  // Parenthesis needed to be able to use cases as type parameters because they need to be `Class cases`
  // https://docs.scala-lang.org/scala3/reference/enums/desugarEnums.html
  enum ExprType:
    case Tiny()
    case Basic()
    case Full()
  import ExprType.*

  enum CalcOutput derives ReadWriter:
    case Error(msg: String)
    case Ok(contents: List[Content])

  enum Content derives ReadWriter:
    case GraphvizFigure(title: String, source: String)
    case PlainText(title: String, text: String)

  trait ExprHandler[K <: ExprType]:
    def process(e: String): CalcOutput

  given ExprHandler[Tiny] with
    import CalcOutput.*
    import Content.*

    override def process(e: String): CalcOutput =
      def go: Try[CalcOutput] = Try:
        val figure = TinyDriver.render(e).get
        val value = TinyDriver.evaluate(e).get
        Ok(
          List(
            GraphvizFigure("AST", figure),
            PlainText("Evaluated", value.toString)
          )
        )
      go.unwrap

  given ExprHandler[Basic] with
    import CalcOutput.*
    import Content.*

    override def process(e: String): CalcOutput =
      def go: Try[CalcOutput] = Try:
        val expr = BasicDriver.parse(e).get
        val value = BasicDriver.evaluate(expr)
        val figure = expr.toGraphviz
        Ok(
          List(
            GraphvizFigure("AST", figure),
            PlainText("Evaluated", value.toString)
          )
        )
      go.unwrap

  given ExprHandler[Full] with
    import CalcOutput.*
    import Content.*

    override def process(e: String): CalcOutput =
      def go: Try[CalcOutput] = Try:
        val block = FullDriver(FullEvaluator.Context.empty).parseBlock(e).get
        val evaluated = FullDriver.evaluateBlock(block).get
        val resultStr = evaluated.defs.map((n, v) => s"$n = $v").appended(evaluated.result.toString).mkString("\n")
        val expr = block.expr
        val constfolded = ConstFoldSimplifier.simplify(expr)
        val algebraic = AlgebraicSimplifier.simplify(expr)
        val simplified = FullSimplifier.simplify(expr)
        Ok(
          List(
            GraphvizFigure("AST", expr.toGraphviz),
            PlainText("Evaluated", resultStr),
            PlainText("Constfolded", constfolded.show),
            GraphvizFigure("Constfolded (AST)", constfolded.toGraphviz),
            PlainText("Algebraic Simplified", algebraic.show),
            GraphvizFigure("Algebraic Simplified (AST)", algebraic.toGraphviz),
            PlainText("Fully Simplified", simplified.show),
            GraphvizFigure("Fully Simplified (AST)", simplified.toGraphviz)
          )
        )
      go.unwrap

  extension (mmx: Try[CalcOutput])
    def unwrap: CalcOutput = mmx match
      case Failure(exception) => CalcOutput.Error(exception.toString)
      case Success(value)     => value
