package chess.async

import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext
import scala.language.{implicitConversions, postfixOps}
import scalaz.concurrent._

trait ChessIO {
  import java.io.{File, PrintWriter}

  def printToFile(f: File)(op: PrintWriter => Unit) {
    val p = new PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def randomFile =
    File.createTempFile("asd", ".result")
}

object AsyncChessSolver extends ChessIO {
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
    @inline def isSafe(pos: Cell, target: Cell) = {
      val d = pos.diff(target)
      !(d.equal((1,0)) || d.equal((0, 1)) || d.equal((1, 1)))
    }
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

  type Cell = (Int, Int)

  implicit class RichCell(val c: Cell) extends AnyVal {
    @inline def row = c._1
    @inline def col = c._2
    @inline def diff(c2: Cell): Cell =
      (abs(c._1 - c2._1), abs(c._2 - c2._2))
    @inline def equal(c2: Cell): Boolean =
      c._1 == c2._1 && c._2 == c2._2
    @inline def inRowOrCol(c2: Cell): Boolean =
      c._1 == c2._1 || c._2 == c2._2
    @inline def inDiagonal(c2: Cell): Boolean = {
      val d = diff(c2)
      d._1 == d._2
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

    /* Checking for each piece if the new move is possible.*/
    @inline def isValidPosition = {
      val target = d.l.head
      d.l.tail forall { m =>
        m._1.isSafe(m._2, target._2) &&
          /* This check is needed since the target piece may
           * be a different one from the others in the board.*/
          target._1.isSafe(target._2, m._2) &&
            m._2 != target._2
      }
    }

    override def toString: String = d.l.mkString(" - ")

    /* Print a decision in a pretty way, but it is a very
     * expensive operation. By default, use another representation. */
    def prettyPrint(b: Board): String = {
      val cells: Map[Cell, String] =
        d.l.map(d => d._2 -> d._1.toString).toMap

      "\n" + genCells(b.n, b.m).map(c => (c, cells.getOrElse(c, " ")))
        .foldLeft("")(
          (acc: String, t: (Cell, String)) => {
            val end = if(t._1.col == b.m) "|\n" else ""
            acc + "|" + t._2 + end
          }
        )
    }
  }

  val inhabitable = 0.toByte

  implicit class TrieMapLikeSet[T](val m: TrieMap[T, Byte]) extends AnyVal {
    /* Return if a element is in the set. Thread-safe operation. */
    @inline def containsOrPut(e: T): Boolean =
      m.putIfAbsent(e, inhabitable).isEmpty
  }

  object TrieMapLikeSet {
    def empty[T]: TrieMapLikeSet[T] =
      TrieMapLikeSet(TrieMap.empty[T, Byte])
  }

  type Visited = TrieMapLikeSet[Decision]

  /* For each decision, check all the possible moves, avoiding all the
   * paths that have already been taken before. Then, mark it as visited.*/
  def decide(i: Iterator[Decision], p: Piece, mem: Visited, d: Board) =
    for {
      decision <- i
      cell <- genCells(d.n, d.m)
      nextDecision = decision add (p, cell)
      /* Order of the conditions matter, don't modify */
      if  (nextDecision isValidPosition) &&
          (mem containsOrPut nextDecision)
    } yield nextDecision

  def onlyPieces(ps: (Int, Piece)) =
    Iterator.tabulate(ps._1)(_ => ps._2)
  def expand(cg: ChessGroups) = cg.flatMap(onlyPieces)

  def genCells(n: Int, m: Int): Iterator[Cell] = for {
    r <- Iterator.range(1, n + 1)
    c <- Iterator.range(1, m + 1)
  } yield (r, c)

  def colCells(n: Int, m: Int): Iterator[Cell] = for {
    c <- Iterator.range(1, m + 1)
  } yield (n, c)

  def seed(p: Piece, row: Int, m: Int): Iterator[Decision] =
    colCells(row, m).map[Decision](c => List(p -> c))

  type ChessGroup = (Int, Piece)
  type ChessGroups = Seq[ChessGroup]

  import java.io.File

  def solve(cg: ChessGroups, b: Board)
           (implicit ec: ExecutionContext): Future[Int] = {
    val pieces = expand(cg).sortBy(_.difficulty).reverse
    val mem = TrieMapLikeSet.empty[Decision]
    val files = TrieMap.empty[Cell, File]

    import java.io.PrintWriter
    val seeds = Vector.range(1, b.n + 1) map (r => Future {
      //val cell = (r, c.col)
      val outputFile = randomFile
      //files.put(cell, outputFile)
      val p = new PrintWriter(outputFile)
      p.println(s"This is thread with assigned row $r")
      val res = pieces.tail.foldLeft(seed(pieces.head, r, b.m)) {
        (decisions, piece) =>
          decide(decisions, piece, mem, b)
      }.map(d => {p.println(d); d}).foldLeft(0) (
        (acc: Int, _: Decision) => acc + 1
      )
      p.close()
      res
    })

    Future.gatherUnordered(seeds).map(_.sum)
  }
}

object ChessApp extends App {

  import AsyncChessSolver._
  //val problem = Vector((2, King), (2, Queen), (2, Bishop), (1, Knight))
  val problem = Vector((2, King), (2, Queen))
  val board = (7, 7)

  import scala.concurrent.ExecutionContext.Implicits.global


  val start = System.currentTimeMillis()
  val solutions = solve(problem, board)

  val numSolutions = solutions.run
  val end = System.currentTimeMillis()
  val runtime = end - start

  println(s"Number of solutions: $numSolutions")
  println(s"Runtime: ${runtime}ms")
}
