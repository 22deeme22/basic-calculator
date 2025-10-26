package cs214

import calculator.Expr

abstract class Parser[E <: Expr](source: List[Token]):
  assert(source.nonEmpty)

  import Parser.*
  import AssocKind.*
  import ParseRule.*

  lazy val rawSource: String =
    if prev ne null then prev.pos.source
    else remaining.head.pos.source

  lazy val initParseTable: ParseTable[E]

  private var myTable: ParseTable[E] | Null = null

  def setTable(table: ParseTable[E]): this.type =
    myTable = table
    this

  def currentTable: ParseTable[E] =
    if myTable eq null then
      setTable(initParseTable)
    myTable

  def inNextLevel[T](op: => T): T =
    val saved = currentTable
    try
      setTable(saved.tail)
      op
    finally setTable(saved)

  def inInitialLevel[T](op: => T): T =
    val saved = currentTable
    try
      setTable(initParseTable)
      op
    finally initParseTable

  var prev: Token = null
  var remaining: List[Token] = source

  def eof: Boolean = remaining.isEmpty

  def peek: Token = remaining.head

  def peekOption: Option[Token] = remaining.headOption

  def issueError(msg: String, pos: Pos = null): Left[Error, Nothing] =
    def getPos = remaining match
      case Nil       => prev.pos
      case head :: _ => head.pos
    val pos1 = if pos ne null then pos else getPos
    val err = Error(msg).withPos(pos1)
    Left(err)

  def issueEofError(expected: String): Left[Error, Nothing] =
    issueError(s"unexpected EOF, expecting $expected", pos = Pos(rawSource, rawSource.length - 1, 1))

  def forward(): Token =
    val result = peek
    remaining = remaining.tail
    prev = result
    result

  def expect(tpe: TokenType, what: String): Result[Token] =
    peekOption match
      case None => issueEofError(what)
      case Some(tok) if tok.tpe == tpe =>
        forward()
        Right(tok)
      case Some(tok) => issueError(s"unexpected token, expecting $what")

  def expectEOF: Result[Unit] =
    def getPos =
      val (head :: rem) = remaining: @unchecked
      rem.foldLeft(head.pos)(_ -- _.pos)

    if eof then Right(()) else issueError("the remaining text cannot be parsed", pos = getPos)

  def attempt[E, A, B](op: => Either[E, A])(other: => Either[E, B]): Either[E, (A | B)] =
    val savedTable = currentTable
    val savedPrev = prev
    val savedRemaining = remaining
    op match
      case Left(e) =>
        myTable = savedTable
        prev = savedPrev
        remaining = savedRemaining
        other
      case x =>
        x

  def parseExprInNextLevel: Result[E] = inNextLevel(parseExpr)

  def parseExpr: Result[E] = currentTable match
    case Nil => parseAtom
    case Unary(rules) :: rem => peekOption match
        case None => issueEofError("start of expression")
        case Some(token) => rules.find(rule => rule.tok == token.tpe) match
            case Some(UnaryOp(_, cons)) =>
              forward()
              parseExpr.map(e => cons(e).withPos(token.pos -- e.pos))
            case None => parseExprInNextLevel
    case Binary(assoc, rules) :: rem =>
      def goLeft(acc: E): Result[E] =
        peekOption match
          case None => Right(acc)
          case Some(token) => rules.find(_.tok == token.tpe) match
              case Some(BinaryOp(_, cons)) =>
                forward()
                parseExprInNextLevel.flatMap(e => goLeft(cons(acc, e).withPos(acc.pos -- e.pos)))
              case None => Right(acc)

      def goRight(e: E, cont: E => E): Result[E] = peekOption match
        case None => Right(cont(e))
        case Some(token) => rules.find(_.tok == token.tpe) match
            case Some(BinaryOp(_, cons)) =>
              forward()
              parseExprInNextLevel.flatMap(e1 => goRight(e1, r => cont(cons(e, r).withPos(e.pos -- r.pos))))
            case None => Right(cont(e))

      assoc match
        case LeftAssoc =>
          parseExprInNextLevel.flatMap(goLeft)
        case RightAssoc =>
          parseExprInNextLevel.flatMap(goRight(_, e => e))

  def parse: Result[E] =
    parseExpr.flatMap(e => expectEOF.map(_ => e))

  def parseAtom: Result[E]

object Parser:
  import Printers.*

  case class Error(msg: String) extends RuntimeException with Pos.WithPos:
    override def toString(): String = this.show(msg :: Nil)

  type Result[+X] = Either[Error, X]

  case class UnaryOp[E <: Expr](tok: TokenType, constructor: E => E)
  case class BinaryOp[E <: Expr](tok: TokenType, constructor: (E, E) => E)

  enum AssocKind:
    case LeftAssoc
    case RightAssoc
  import AssocKind.*

  enum ParseRule[E <: Expr]:
    case Unary(opRules: List[UnaryOp[E]])
    case Binary(assoc: AssocKind, opRules: List[BinaryOp[E]])
  import ParseRule.*

  type ParseTable[E <: Expr] = List[ParseRule[E]]
