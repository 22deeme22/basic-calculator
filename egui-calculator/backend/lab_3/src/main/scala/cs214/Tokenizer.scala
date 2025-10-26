package cs214

class Tokenizer(source: String):
  import Tokenizer.*

  private var start: Int = 0
  private var current: Int = 0

  def eof: Boolean = current >= source.length

  def currentPos: Pos = Pos(source, start, current - start)

  def makeToken(tpe: TokenType): Token =
    val content = source.substring(start, current)
    Token(tpe, content).withPos(currentPos)

  def emitToken(tpe: TokenType): Right[Error, Token] = Right(makeToken(tpe))

  def issueEOF: Left[Error, Token] = Left(EOF().withPos(currentPos))

  def issueSyntaxError(msg: String): Left[Error, Token] = Left(SyntaxError(msg).withPos(currentPos))

  def peek: Char = source(current)

  def forward(): Char =
    val ch = peek
    current += 1
    ch

  def consumeWhitespaces(): Unit =
    @annotation.tailrec
    def go(): Unit =
      if eof || !peek.isWhitespace then ()
      else
        forward()
        go()
    go()

  def parseLiteral(isDot: Boolean): Result[Token] =
    var seenDot: Boolean = isDot
    var seenDigit: Boolean = false

    def mayEmitLiteral =
      if isDot && !seenDigit then issueSyntaxError("at least one digit should follow a single dot")
      else emitToken(TokenType.Literal)

    @annotation.tailrec
    def go(): Result[Token] =
      if eof then mayEmitLiteral
      else
        peek match
          case '.' if seenDot => issueSyntaxError("at most one dot in a number")
          case '.' =>
            seenDot = true
            forward()
            go()
          case ch if ch.isDigit =>
            seenDigit = true
            forward()
            go()
          case _ => mayEmitLiteral
    go()

  def parseVar(): Result[Token] =
    def emit = emitToken(TokenType.Var)
    @annotation.tailrec
    def go(): Result[Token] =
      if eof then emit
      else
        peek match
          case ch if ch.isLetterOrDigit =>
            forward()
            go()
          case _ => emit
    go()

  def nextToken(): Result[Token] =
    consumeWhitespaces()
    if eof then issueEOF
    else
      start = current
      val ch = forward()
      ch match
        case '+'                           => emitToken(TokenType.Plus)
        case '-'                           => emitToken(TokenType.Minus)
        case '*'                           => emitToken(TokenType.Star)
        case '/'                           => emitToken(TokenType.Slash)
        case '('                           => emitToken(TokenType.LeftParen)
        case ')'                           => emitToken(TokenType.RightParen)
        case '='                           => emitToken(TokenType.Equal)
        case ch if ch == '.' || ch.isDigit => parseLiteral(ch == '.')
        case ch if ch.isLetter             => parseVar()
        case _                             => issueSyntaxError(s"unrecognizable character `$ch`")

object Tokenizer:
  import Printers.*

  sealed trait Error extends RuntimeException with Pos.WithPos
  case class EOF() extends Error:
    override def toString(): String = this.show("unexpected EOF" :: Nil)
  case class SyntaxError(msg: String) extends Error:
    override def toString(): String = this.show(msg :: Nil)

  type Result[X] = Either[Error, X]

  def tokenize(source: String): Result[List[Token]] =
    val tokenizer = new Tokenizer(source)
    @annotation.tailrec
    def go(xs: List[Token]): Result[List[Token]] =
      tokenizer.nextToken() match
        case Left(EOF()) => Right(xs.reverse)
        case Left(err)   => Left(err)
        case Right(tok) =>
          go(tok :: xs)
    go(Nil)
