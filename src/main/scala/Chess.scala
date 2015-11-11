import scala.language.implicitConversions

object Chess {
  sealed trait Piece {
    /** Assigned according to the
      * power of the piece. Used to
      * backtrack before on pieces
      * which imply more constraints. */
    val priority: Int

    /** Return if the piece in `pos`
      * can take the piece in `target`.
      * Assume they are different cells. */
    def canBeTaken(pos: Cell, target: Cell): Boolean
  }

  object Piece {
    def apply(s: String): Piece =
      s match {
        case "rook" => Rook
        case "king" => King
        case "queen" => Queen
        case "bishop" => Bishop
        case "knight" => Knight
      }
  }

  import Math.abs

  case object Rook extends Piece {
    val priority = 4

    override def toString: String = "R"
    @inline def canBeTaken(pos: Cell, target: Cell) =
      pos.inRowOrCol(target)
  }

  case object King extends Piece {
    val priority = 1

    override def toString: String = "K"
    @inline def canBeTaken(pos: Cell, target: Cell) =
      pos.diff(target).equal((1, 1))
  }

  case object Queen extends Piece {
    val priority = 5

    override def toString: String = "Q"
    @inline def canBeTaken(pos: Cell, target: Cell) =
      pos.inRowOrCol(target) || pos.inDiagonal(target)
  }

  case object Bishop extends Piece {
    val priority = 3

    override def toString: String = "B"
    @inline def canBeTaken(pos: Cell, target: Cell) =
      pos.inDiagonal(target)
  }

  case object Knight extends Piece {
    val priority = 2

    override def toString: String = "N"
    @inline def canBeTaken(pos: Cell, target: Cell) = {
      val d = pos.diff(target)
      d.equal((1, 2)) || d.equal((2, 1))
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
    @inline def inDiagonal(c2: Cell): Boolean =
      equal(diff(c2))
  }
}
