package cs214

import scala.io

object Printers:
  def showPos(pos: Pos, msg: List[String], contexts: Int = 0): String =
    val lines = pos.source.linesWithSeparators.toList

    def locateIdx(idx: Int): (Int, Int) =
      @annotation.tailrec
      def go(current: Int, line: Int, col: Int): (Int, Int) =
        if current <= 0 then (line, col)
        else
          val currentLine = lines(line)
          if col + 1 >= currentLine.length then
            if line + 1 >= lines.length then
              go(0, line, col)
            else
              go(current - 1, line + 1, 0)
          else go(current - 1, line, col + 1)
      go(idx, 0, 0)

    val sb = StringBuilder()
    val (startLine, startCol) = locateIdx(pos.start)
    val (endLine, endCol) = locateIdx(pos.start + pos.length - 1)

    def leftPad(s: String, width: Int, padChar: String = " "): String =
      if s.length >= width then s
      else
        val delta = width - s.length
        val padStr: String = padChar.toString * delta
        padStr + s

    def outputSourceLine(lineno: Int, mark: Option[(Int, Int)], msg: List[String]): Unit =
      val width = lines.length.toString.length
      val prefix = s" ${leftPad((lineno + 1).toString, width, padChar = "0")} | "
      sb ++= prefix
      sb ++= lines(lineno)
      if !lines(lineno).endsWith("\n") then sb += '\n'
      mark match
        case Some((start, len)) =>
          val spaceLen = prefix.length + start
          val markLen = if len <= 0 then 1 else len
          sb ++= " " * spaceLen
          sb ++= "^" * markLen
          sb += '\n'
          msg foreach { m =>
            sb ++= " " * spaceLen
            sb ++= m
            sb += '\n'
          }
        case _ =>

    val contextStart = (startLine - contexts) max 0
    val contextEnd = (endLine + 1 + contexts) min lines.length

    val before = contextStart until startLine
    val content = startLine until (endLine + 1)
    val after = (endLine + 1) until contextEnd

    def outputBefore =
      before foreach { idx => outputSourceLine(idx, None, Nil) }

    def outputAfter =
      after foreach { idx => outputSourceLine(idx, None, Nil) }

    def outputStart =
      if startLine == endLine then
        outputSourceLine(startLine, Some((startCol, endCol - startCol + 1)), msg)
      else
        outputSourceLine(startLine, Some((startCol, lines(startLine).length - startCol)), Nil)

    def outputMiddle =
      (startLine + 1) until endLine foreach { idx => outputSourceLine(idx, Some((0, lines(idx).length)), Nil) }

    def outputEnd =
      if startLine != endLine then
        outputSourceLine(endLine, Some((0, endCol)), msg)

    outputBefore; outputStart; outputMiddle; outputEnd; outputAfter
    sb.result()

  extension (x: Pos.WithPos)
    def show(msg: List[String], contexts: Int = 0): String = showPos(x.pos, msg, contexts = contexts)
