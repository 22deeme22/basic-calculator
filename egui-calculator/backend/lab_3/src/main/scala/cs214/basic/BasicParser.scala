package cs214
package basic

import Parser.*
import AssocKind.*
import ParseRule.*

import calculator.basic.*

class BasicParser(source: List[Token]) extends Parser[BasicExpr](source):
  import BasicExpr.*

  lazy val initParseTable: ParseTable[BasicExpr] = List(
    Binary(LeftAssoc, List(BinaryOp(TokenType.Minus, Minus(_, _)), BinaryOp(TokenType.Plus, Add(_, _)))),
    Binary(LeftAssoc, List(BinaryOp(TokenType.Star, Mul(_, _)), BinaryOp(TokenType.Slash, Div(_, _)))),
    Unary(List(UnaryOp(TokenType.Minus, e => Neg(e))))
  )

  def parseAtom: Result[BasicExpr] =
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
          case _ => issueError("unexpected token, expecting a literal or a left parenthesis `(`")
