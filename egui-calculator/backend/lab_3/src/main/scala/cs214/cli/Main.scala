package cs214.cli

import util.{Try, Failure, Success}
import calculator.full.*
import cs214.full.*
import cs214.full.FullPrinter.show
import cs214.tiny.TinyDriver
import cs214.basic.BasicDriver

class REPL(initMode: REPL.Mode):
  import REPL.*
  import Mode.*

  lazy val interactors: Map[Mode, Interactor] = Map(
    EvalTiny -> new EvalTinyInteractor,
    EvalBasic -> new EvalBasicInteractor,
    EvalFull -> new EvalFullInteractor,
    ConstFold -> new ConstFoldInteractor,
    Algebr -> new ArithInteractor,
    Simp -> new SimpInteractor
  )

  private var myMode: Mode = initMode

  def setMode(m: Mode): this.type =
    myMode = m
    this

  private def getInteractor: Interactor = interactors(myMode)

  def interact(input: String): Unit =
    if !input.isEmpty then
      val res = Try:
        getInteractor.interact(input)
      res match
        case Failure(e) => println(s"Error:\n${e.toString}")
        case Success(_) => ()

  private def changeMode(m: Mode) =
    setMode(m)
    println(s"Mode has been changed to $m")

  def loop(): Unit =
    val source = scala.io.StdIn.readLine(text = "> ")
    var isEnded = false

    def action(cmd: String): Unit = cmd match
      case REPL.ModeString(mode) => changeMode(mode)
      case "quit" | "exit"       => isEnded = true
      case cmd                   => println(s"Unknown command: $cmd")

    source.trim match
      case s if s.startsWith(":") => action(s.substring(1))
      case source                 => interact(source)
    if !isEnded then loop()

object REPL:
  enum Mode:
    case EvalTiny
    case EvalBasic
    case EvalFull
    case ConstFold
    case Algebr
    case Simp
    case SmallStep

  object Mode:
    def fromString(mode: String): Option[Mode] = mode match
      case "tiny"      => Some(EvalTiny)
      case "basic"     => Some(EvalBasic)
      case "full"      => Some(EvalFull)
      case "constfold" => Some(ConstFold)
      case "algebraic" => Some(Algebr)
      case "simp"      => Some(Simp)
      case _           => None

  object ModeString:
    def unapply(s: String): Option[Mode] = Mode.fromString(s)

  trait Interactor:
    def interact(input: String): Unit

  class NotSupportedInteractor(name: String) extends Interactor:
    def interact(input: String): Unit = throw NotImplementedError(name)

  class EvalTinyInteractor extends Interactor:
    def interact(input: String): Unit =
      val result = TinyDriver.evaluate(input).get
      println(result)

  class EvalBasicInteractor extends Interactor:
    def interact(input: String): Unit =
      val result = BasicDriver.evaluate(input).get
      println(result)

  class EvalFullInteractor extends Interactor:
    var myCtx = FullEvaluator.Context.empty

    def interact(input: String): Unit =
      val result = FullDriver(myCtx).parseDef(input).get
      result match
        case (name, e) =>
          val result = FullDriver(myCtx).evaluate(e).get
          result match
            case FullEvaluator.Result.Ok(v) =>
              println(s"$name = $v")
              myCtx = FullEvaluator.Context.cons(name, v, myCtx)
            case err => err.get
        case e => println(FullDriver(myCtx).evaluate(input).get)

  class ConstFoldInteractor extends Interactor:
    def interact(input: String): Unit =
      val result = ConstFoldDriver(FullEvaluator.Context.empty).simplify(input).get
      println(result.show)

  class ArithInteractor extends Interactor:
    def interact(input: String): Unit =
      val result = AlgebraicDriver(FullEvaluator.Context.empty).simplify(input).get
      println(result.show)

  class SimpInteractor extends Interactor:
    def interact(input: String): Unit =
      val result = FullDriver(FullEvaluator.Context.empty).simplify(input).get
      println(result.show)

@main def main(args: String*): Unit =
  var theMode: String = "tiny"
  args.toList match
    case Nil      =>
    case m :: Nil => theMode = m
    case _        => throw IllegalArgumentException("too many arguments")
  REPL.Mode.fromString(theMode) match
    case None => throw IllegalArgumentException(s"unknown mode: ${args.head}")
    case Some(args) =>
      val repl = new REPL(args)
      repl.loop()
