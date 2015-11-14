package cpu.sync

import scala.collection.mutable.{Set => MutableSet}
import scala.language.implicitConversions

object SyncChessSolver {
  sealed trait Piece {
    /** Assigned according to the number
      * of possible moves that are removed
      * from the search space when the piece
      * is placed in the board. Order:
      * Queen > Rook > Bishop > Knight > King */
    val difficulty: Int

    /** Return if the piece in `pos`
      * cannot take the piece in `target`.
      * Assume they are different cells. */
    @inline def isSafe(pos: Cell, target: Cell): Boolean
  }

  import Math.abs

  case object Rook extends Piece {
    val difficulty = 4

    override def toString: String = "R"
    @inline def isSafe(pos: Cell, target: Cell) =
      !pos.inRowOrCol(target)
  }

  case object King extends Piece {
    val difficulty = 1

    override def toString: String = "K"
    @inline def isSafe(pos: Cell, target: Cell) =
      !pos.diff(target).equal((1, 1))
  }

  case object Queen extends Piece {
    val difficulty = 5

    override def toString: String = "Q"
    @inline def isSafe(pos: Cell, target: Cell) =
      !(pos.inRowOrCol(target) || pos.inDiagonal(target))
  }

  case object Bishop extends Piece {
    val difficulty = 3

    override def toString: String = "B"
    @inline def isSafe(pos: Cell, target: Cell) =
      !pos.inDiagonal(target)
  }

  case object Knight extends Piece {
    val difficulty = 2

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


  /* These are value classes to give a more idiomatic
   * and clear way to call the fields of the tuples
   * without case classes, eventually freeing allocations. */

  type Move = (Piece, Cell)

  implicit class RichMove(val m: Move) extends AnyVal {
    @inline def piece = m._1
    @inline def cell = m._2
  }

  implicit class Board(val t: (Int, Int)) extends AnyVal {
    @inline def n = t._1
    @inline def m = t._1
  }

  /* It cannot be a value class because SIP-15 does
   * not allow them to define `hashCode` and `equals`.*/
  implicit class RichHashList[T](val l: List[T]) {
    override def toString = l.toString()
    /* Override using an unorderedHash because using a `Set`
     * will cause bad performance and it is required to check
     * efficiently that two lists have the same positions. */
    override def hashCode() =
      scala.util.hashing.MurmurHash3.unorderedHash(l)
    override def equals(that: Any) = that match {
      case _: RichHashList[T] => hashCode() == that.hashCode()
      case _ => false
    }
  }

  type Decision = RichHashList[Move]

  /* This class is a value class, which is key to the performance
   * of the solution. The methods `hashCode` and `equals` couldn't
   * be defined here because the specification SIP-15 forbids it. */
  implicit class RichDecision(val d: Decision) extends AnyVal {
    @inline def add(m: Move): Decision = m :: d.l
    @inline def isNotVisited(mem: Visited): Boolean = !mem.contains(d)

    /* Checking for each piece if the new move is possible.*/
    @inline def isValidPosition(target: Move) = d.l forall { m =>
      m.cell != target.cell &&
        m.piece.isSafe(m.cell, target.cell) &&
        /* This check is needed since the target piece may
         * be a different one from the others in the board.*/
        target.piece.isSafe(target.cell, m.cell)
    }
  }

  type Visited = MutableSet[Decision]

  /* For each decision, check all the possible moves, avoiding all the
   * paths that have already been taken before. Then, mark it as visited.*/
  def decide(i: Iterator[Decision], p: Piece, mem: Visited, d: Board) =
    for {
      decision <- i
      cell <- genCells(d.n, d.m)
      move = (p, cell)
      nextDecision = decision add move
      if (decision isValidPosition move) &&
        (nextDecision isNotVisited mem)
    } yield { mem += nextDecision; nextDecision}

  def onlyPieces(ps: (Int, Piece)) =
    Iterator.tabulate(ps._1)(_ => ps._2)
  def expand(cg: ChessGroups) = cg.flatMap(onlyPieces)

  def genCells(n: Int, m: Int): Iterator[Cell] = for {
    r <- Iterator.range(1, n + 1)
    c <- Iterator.range(1, m + 1)
  } yield Cell(r, c)

  def seed(p: Piece, d: Board) =
    genCells(d.n, d.m).map[Decision]{
      c => List(p -> c)
    }

  type ChessGroup = (Int, Piece)
  type ChessGroups = Seq[ChessGroup]

  /* Sort the pieces by difficulty. Place first the most difficult
   * ones because they restrict more the possible movements in the
   * board. Mark every taken path as visited in order to avoid
   * try solutions with moves that have already been inspected.
   * Store those paths in a **mutable** Set in order to share the
   * latest state in each iteration. It is not immutable because
   * there is now way to share it with each independent iteration. */
  def solve(cg: ChessGroups, dim: Board): Iterator[Decision] = {
    val pieces = expand(cg).sortBy(_.difficulty)
    val mem = MutableSet.empty[Decision]
    pieces.tail.foldLeft(seed(pieces.head, dim)) {
      (decisions, piece) =>
        decide(decisions, piece, mem, dim)
    }
  }
}

object ChessApp extends App {
  import SyncChessSolver._
  val problem = Vector((7, Queen))
  val board = (7, 7)

  val start = System.currentTimeMillis()
  val solutions = solve(problem, board).toVector
  val end = System.currentTimeMillis()
  val runtime = end - start

  def corrector(solutions: Vector[Decision]): Vector[(Decision, Boolean)] =
    solutions map { d2 =>
      d2 -> d2.l.tails.filter(_.nonEmpty).forall{
        l => l.tail.forall(m => m.piece.isSafe(m.cell, l.head.cell))
      }
    }

  //println(corrector(solutions) mkString "\n")
  println(s"Length: ${solutions.size}")
  println(s"Runtime: $runtime")
}
