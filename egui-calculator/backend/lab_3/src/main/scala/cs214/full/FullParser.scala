package cs214
package full

import Parser.*
import AssocKind.*
import ParseRule.*

import calculator.full.*

class FullParser(source: List[Token]) extends Parser[FullExpr](source):
  import FullExpr.*

  lazy val initParseTable: ParseTable[FullExpr] = List(
    Binary(LeftAssoc, List(BinaryOp(TokenType.Minus, Minus(_, _)), BinaryOp(TokenType.Plus, Add(_, _)))),
    Binary(LeftAssoc, List(BinaryOp(TokenType.Star, Mul(_, _)), BinaryOp(TokenType.Slash, Div(_, _)))),
    Unary(List(UnaryOp(TokenType.Minus, e => Neg(e))))
  )

  def parseAtom: Result[FullExpr] =
    peekOption match
      case None => issueEofError("start of a number or a left parenthesis `(`")
      case Some(tok) => tok.tpe match
          case TokenType.LeftParen =>
            forward()
            inInitialLevel:
              parseExpr.flatMap(e => expect(TokenType.RightParen, "right parenthesis `)`").map(_ => e))
          case TokenType.Literal =>
            forward()
            Right(Number(tok.content.toDouble).withPos(tok.pos))
          case TokenType.Var =>
            forward()
            Right(FullExpr.Var(tok.content).withPos(tok.pos))
          case _ => issueError("unexpected token, expecting a literal or a left parenthesis `(`")

  def parseAssign: Result[(String, FullExpr)] =
    (for
      ident <- expect(TokenType.Var, "identifier")
      _ <- expect(TokenType.Equal, "equal sign")
      e <- parseExpr
    yield (ident.content, e))
      .flatMap(e => expectEOF.map(_ => e))

  def parseDef: Result[(String, FullExpr) | FullExpr] =
    val result = attempt(parseAssign)(parseExpr)
    result.flatMap(e => expectEOF.map(_ => e))
