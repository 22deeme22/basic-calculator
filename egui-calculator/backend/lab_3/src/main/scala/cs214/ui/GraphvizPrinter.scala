package cs214.ui
import calculator.Expr
import calculator.tiny.TinyExpr
import calculator.basic.BasicExpr
import calculator.full.FullExpr

object GraphvizPrinter:
  enum Rope[T]:
    case Empty()
    case Single(t: T)
    case Cons(t: T, r: Rope[T])
    case ConsN(ts: List[T], r: Rope[T])
    case Concat(r1: Rope[T], r2: Rope[T])

    def toList(base: List[T] = Nil): List[T] =
      this match
        case Empty()        => base
        case Single(t)      => t :: base
        case Cons(t, r)     => t :: r.toList(base)
        case ConsN(ts, r)   => ts ++ r.toList(base)
        case Concat(r1, r2) => r1.toList(r2.toList(base))

  case class Graph(nodes: Rope[String], edges: Rope[String]):
    def mkString: String =
      val nodeLines = nodes.toList().mkString("\n")
      val edgeLines = edges.toList().mkString("\n")
      f"digraph {\n${nodeLines}\n${edgeLines}\n}"

  object Graph:
    def concat(g1: Graph, g2: Graph): Graph =
      Graph(Rope.Concat(g1.nodes, g2.nodes), Rope.Concat(g1.edges, g2.edges))

  object TinyExprPrinter extends GraphvizPrinter[TinyExpr]:
    import TinyExpr.*
    import Rope.*

    def rec(e: TinyExpr): (String, Graph) = e match
      case Number(v) =>
        val (id, node) = mkNode(f"$v", "circle")
        (id, Graph(Single(node), Empty()))
      case Add(e1, e2)   => op("+", e1, e2)
      case Minus(e1, e2) => op("-", e1, e2)
      case Mul(e1, e2)   => op("*", e1, e2)
      case Neg(e)        => op("-", e)

  object BasicExprPrinter extends GraphvizPrinter[BasicExpr]:
    import BasicExpr.*
    import Rope.*

    def rec(e: BasicExpr): (String, Graph) = e match
      case Number(v) =>
        val (id, node) = mkNode(f"$v", "circle")
        (id, Graph(Single(node), Empty()))
      case Add(e1, e2)   => op("+", e1, e2)
      case Minus(e1, e2) => op("-", e1, e2)
      case Mul(e1, e2)   => op("*", e1, e2)
      case Div(e1, e2)   => op("/", e1, e2)
      case Neg(e)        => op("-", e)

  object FullExprPrinter extends GraphvizPrinter[FullExpr]:
    import FullExpr.*
    import Rope.*

    def rec(e: FullExpr): (String, Graph) = e match
      case Number(v) =>
        val (id, node) = mkNode(f"$v", "circle")
        (id, Graph(Single(node), Empty()))
      case Add(e1, e2)   => op("+", e1, e2)
      case Minus(e1, e2) => op("-", e1, e2)
      case Mul(e1, e2)   => op("*", e1, e2)
      case Div(e1, e2)   => op("/", e1, e2)
      case Neg(e)        => op("-", e)
      case Var(name) =>
        val (id, node) = mkNode(f"$name", "circle")
        (id, Graph(Single(node), Empty()))

  extension [E <: Expr](e: E)
    def toGraphviz(using printer: GraphvizPrinter[E]): String = printer.toGraphviz(e)

  given tinyPrinter: GraphvizPrinter[TinyExpr] = TinyExprPrinter
  given basicPrinter: GraphvizPrinter[BasicExpr] = BasicExprPrinter
  given fullPrinter: GraphvizPrinter[FullExpr] = FullExprPrinter

trait GraphvizPrinter[E <: Expr]:
  import GraphvizPrinter.*
  import Rope.*

  private var gensym = 0

  def mkNode(label: String, shape: String): (String, String) =
    try
      val id = f"n${gensym}"
      (id, f"${id} [label=\"${label}\", shape=\"${shape}\"]")
    finally gensym = gensym + 1

  def mkEdge(src: String, dst: String): String =
    f"$src -> $dst"

  def op(opStr: String, exprs: E*): (String, Graph) =
    val (id, node) = mkNode(opStr, "square")
    val (ids, graphs) = exprs.map(rec).unzip
    val graph = graphs.reduce(Graph.concat)
    (id, Graph(Cons(node, graph.nodes), ConsN(ids.map(mkEdge(id, _)).toList, graph.edges)))

  def rec(e: E): (String, Graph)

  def toGraphviz(e: E): String =
    gensym = 0
    rec(e)._2.mkString
