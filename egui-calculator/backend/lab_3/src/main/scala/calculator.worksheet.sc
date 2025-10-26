import cs214.tiny.*
import cs214.basic.*
import cs214.full.*
import calculator.tiny.*
import calculator.basic.*
import calculator.full.*

val e1: TinyExpr = TinyDriver.parse("2 + 3").get

// Both should be equal
e1.toString()
TinyPrinter.show(e1)

TinyPrinter.toPolish(e1)

val tinyEvaluator = new TinyEvaluator
tinyEvaluator.evaluate(e1)

val e2: FullExpr = FullDriver(FullEvaluator.Context.empty).parse("y - y - 0.0").get
// Try out some manual test cases here to debug your code
