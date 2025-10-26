package calculator

import calculator.full.FullExpr
import calculator.full.FullExpr.*
import cs214.full.FullPrinter.*
import cs214.full.FullDriverT

object SimpSuite:
  val enableDebugTests = true

  extension (name: String)
    def asVar: Var = Var(name)

  extension (v: Double)
    def asNum: FullExpr = Number(v)

  extension (e: FullExpr)
    def neg: FullExpr = Neg(e)
    def +(other: FullExpr): FullExpr = Add(e, other)
    def -(other: FullExpr): FullExpr = Minus(e, other)
    def *(other: FullExpr): FullExpr = Mul(e, other)
    def /(other: FullExpr): FullExpr = Div(e, other)

trait SimpSuite extends munit.FunSuite:
  def name: String
  def driver: FullDriverT
  def tests: List[(String, FullExpr)]

  for (i, o) <- tests do
    val res = driver.simplify(i).get
    test(f"${name}Simplifier: ${name}Driver.simplify(\"$i\").get is \"${res.show}\" and should be \"${o.show}\" (1pt)"):
      assertEquals(res, o)
