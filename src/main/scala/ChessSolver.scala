import scala.language.implicitConversions

object ChessSolver {
  sealed trait Piece {
    /** Assigned according to the
      * power of the piece. Used to
      * backtrack before on pieces
      * which imply more constraints. */
    val priority: Int

    /** Return if the piece in `pos`
      * can take the piece in `target`.
      * Assume they are different cells. */
    def isSafe(pos: Cell, target: Cell): Boolean
  }

  import Math.abs

  case object Rook extends Piece {
    val priority = 4

    override def toString: String = "R"
    @inline def isSafe(pos: Cell, target: Cell) =
      !pos.inRowOrCol(target)
  }

  case object King extends Piece {
    val priority = 1

    override def toString: String = "K"
    @inline def isSafe(pos: Cell, target: Cell) =
      !pos.diff(target).equal((1, 1))
  }

  case object Queen extends Piece {
    val priority = 5

    override def toString: String = "Q"
    @inline def isSafe(pos: Cell, target: Cell) =
      !(pos.inRowOrCol(target) || pos.inDiagonal(target))
  }

  case object Bishop extends Piece {
    val priority = 3

    override def toString: String = "B"
    @inline def isSafe(pos: Cell, target: Cell) =
      !pos.inDiagonal(target)
  }

  case object Knight extends Piece {
    val priority = 2

    override def toString: String = "N"
    @inline def isSafe(pos: Cell, target: Cell) = {
      val d = pos.diff(target)
      !(d.equal((1, 2)) || d.equal((2, 1)))
    }
  }

  implicit def tupleToCell(t: (Int, Int)): Cell = Cell(t)

  case class Cell(c: (Int, Int)) extends AnyVal {
    @inline def row = c._1
    @inline def col = c._2
    @inline def diff(c2: Cell): Cell =
      (abs(row - c2.row), abs(col - c2.col))
    @inline def equal(c2: Cell): Boolean =
      row == c2.row && col == c2.col
    @inline def inRowOrCol(c2: Cell): Boolean =
      row == c2.row || col == c2.col
    @inline def inDiagonal(c2: Cell): Boolean = {
      val d = diff(c2)
      d.row == d.col
    }
  }

  case class Dimensions(t: (Int, Int)) extends AnyVal {
    def n = t._1
    def m = t._1
  }

  type Move = (Piece, Cell)

  implicit class RichMove(val m: Move) extends AnyVal {
    def piece = m._1
    def cell = m._2
  }

  type Decision = List[Move]

  implicit class RichDecision(val d: Decision) extends AnyVal {
    def add(p: Piece, c: Cell): Decision = (p -> c) :: d
    def isValidWith(target: Cell) = d forall { m =>
      m.cell != target && m.piece.isSafe(m.cell, target)
    }
  }

  type ChessGroups = Vector[(Int, Piece)]

  def genCells(n: Int, m: Int): Iterator[Cell] = for {
    r <- Iterator.range(1, n + 1)
    c <- Iterator.range(1, m + 1)
  } yield Cell(r, c)

  def decide(ds: Iterator[Decision], p: Piece, d: Dimensions) =
    for {
      decision <- ds
      choice <- genCells(d.n, d.m)
      if decision isValidWith choice
    } yield decision add (p, choice)

  def onlyPieces(ps: (Int, Piece)) =
    Iterator.tabulate(ps._1)(_ => ps._2)

  def expand(cg: ChessGroups) = cg.flatMap(onlyPieces)

  def seed(p: Piece, d: Dimensions): Iterator[Decision] =
    genCells(d.n, d.m) map (c => List(p -> c))

  def solve(cg: ChessGroups, dim: Dimensions): Iterator[Decision] = {
    val pieces = expand(cg).sortBy(_.priority)
    pieces.tail.foldLeft(seed(pieces.head, dim)) {
      (decisions, piece) =>
        decide(decisions, piece, dim)
    }
  }
}

object ChessApp extends App {
  import ChessSolver._
  val problem = Vector((7, Queen))
  val dimensions = Dimensions(7, 7)

  val solutions = solve(problem, dimensions).toVector map (_.toSet)
  println(solutions mkString "\n")
  println(s"Length: ${solutions.size}")
}
