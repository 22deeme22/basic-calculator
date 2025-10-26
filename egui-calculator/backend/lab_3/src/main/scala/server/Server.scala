package server

import cs214.Driver
import cask.MainRoutes
import upickle.default._
import calculator.Evaluator
import cs214.Driver
import cs214.full.FullDriver
import calculator.full.FullExpr
import calculator.full.FullEvaluator
import cs214.full.FullPrinter.show
import scala.util.Try

object Server extends MainRoutes {


@cask.get("/compute")

def compute(exprStr: String) = {
  val resultJson = Try {
    val expr: FullExpr = FullDriver(FullEvaluator.Context.empty).parse(exprStr).get
    val res: FullEvaluator.Result = FullDriver(FullEvaluator.Context.empty).evaluate(expr).get
    val resString = res match {
      case FullEvaluator.Result.Ok(v) => v.toString
      case FullEvaluator.Result.DivByZero => "Division by zero!"
      case FullEvaluator.Result.UndefinedVar(n) => " "
    }
    write(Map("parsedExpr" -> expr.show, "result" -> resString))
  }.getOrElse(write(Map("error" -> "Invalid expression")))

  cask.Response(resultJson, headers = Seq("Content-Type" -> "application/json"))
}

  initialize()
}
