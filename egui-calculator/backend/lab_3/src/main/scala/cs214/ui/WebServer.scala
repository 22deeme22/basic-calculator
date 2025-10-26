package cs214.ui

import ExprHandlers.*
import ExprHandlers.ExprType.*

// Needed because cask.MainRoutes is not open
import language.adhocExtensions
import util.{Try, Success, Failure}

object main extends cask.MainRoutes:
  override def host: String = sys.env.getOrElse("CS214_HOST", "127.0.0.1")
  override def port: Int = sys.env.get("CS214_PORT").map(_.toInt).getOrElse(8080)
  final class UnknownExprTypeException() extends Exception {}

  /** Paths where the static content served by the server is stored */
  private val STATIC_PATH = "src/main/www"
  private def HTML_STATIC_FILE =
    cask.model.StaticFile(STATIC_PATH + "/calculator.html", Seq("Content-Type" -> "text/html"))

  // Can't use `enum.valueOf(s)` because they're `Class cases` and not `Singleton cases`. They need to be `Class cases`
  // because they're used as a type parameter in `ExprHandlers.scala`
  // https://docs.scala-lang.org/scala3/reference/enums/desugarEnums.html
  given cask.endpoints.QueryParamReader.SimpleParam[ExprType] = cask.endpoints.QueryParamReader.SimpleParam[ExprType](
    s =>
      s.toLowerCase() match
        case "tiny"  => Tiny()
        case "basic" => Basic()
        case "full"  => Full()
        case _       => throw UnknownExprTypeException()
  )

  @cask.get("/")
  def htmlStaticFile() = HTML_STATIC_FILE

  @cask.staticFiles("/src")
  def srcStaticFiles() = STATIC_PATH

  @cask.getJson("/calc/")
  def getCalc(exprType: ExprType, expr: String) =
    println(s"Got request: $expr, type = $exprType")
    val handler = exprType match
      case Tiny()  => summon[ExprHandler[ExprType.Tiny]]
      case Basic() => summon[ExprHandler[ExprType.Basic]]
      case Full()  => summon[ExprHandler[ExprType.Full]]
    val res = handler.process(expr)
    res

  initialize()
