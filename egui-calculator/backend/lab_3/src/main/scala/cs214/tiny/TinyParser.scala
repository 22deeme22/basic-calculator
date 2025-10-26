package cs214
package tiny

import Parser.*
import AssocKind.*
import ParseRule.*

import calculator.tiny.*

class TinyParser(source: List[Token]) extends Parser[TinyExpr](source):
  import TinyExpr.*

  lazy val initParseTable: ParseTable[TinyExpr] = List(
    Binary(LeftAssoc, List(BinaryOp(TokenType.Minus, Minus(_, _)), BinaryOp(TokenType.Plus, Add(_, _)))),
    Binary(LeftAssoc, List(BinaryOp(TokenType.Star, Mul(_, _)))),
    Unary(List(UnaryOp(TokenType.Minus, e => Neg(e))))
  )

  def parseAtom: Result[TinyExpr] =
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
