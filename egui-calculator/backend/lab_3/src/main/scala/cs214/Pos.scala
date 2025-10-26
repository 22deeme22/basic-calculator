package cs214

/** A position, or a span of text, in the source */
case class Pos(source: String, start: Int, length: Int):
  def --(other: Pos): Pos = Pos(source, start, other.start - start + length)

object Pos:

  /** A trait for objects that can be positioned in the source. */
  trait WithPos:
    private var myPos: Pos | Null = null

    /** Set the position. */
    def setPos(p: Pos): Unit = myPos = p

    /** Set the position and return self. */
    def withPos(p: Pos): this.type =
      setPos(p)
      this

    /** Get the position, asserting it is non-null. */
    def pos: Pos = myPos.nn

    /** Check whether the object has a position. */
    def hasPos: Boolean = myPos ne null
