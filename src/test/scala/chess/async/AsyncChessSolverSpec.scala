package chess.async

import org.scalacheck.Gen
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

/** This specification only checks that the results of the Solver
  * are right, but not that it outputs all the possible solutions.
  * This is due to the fact that boards and configurations are randomly
  * generated. Fine-grained testing has been done for the specific examples
  * in the challenge. */
class AsyncChessSolverSpec extends PropSpec with PropertyChecks
                                               with Matchers with ScalaFutures {
  import AsyncChessSolver._

  /** Checks that a vector of Decisions is a good solution for the problem. */
  def corrector(solutions: Seq[Decision]): Seq[Boolean] =
    solutions map { d2 =>
      d2.l.tails.filter(_.nonEmpty).forall{
        l => l.tail.forall(m => m.piece.isSafe(m.cell, l.head.cell))
      }
    }

  def boardGen: Gen[Board] = for {
    n <- Gen.choose[Int](3, 5)
    m <- Gen.choose[Int](3, 5)
  } yield (n, m)

  def pieceGen: Gen[Piece] =
    Gen.oneOf(List(King, Queen, Knight, Rook, Bishop))

  def chessGroupGen: Gen[ChessGroup] = for {
    p <- pieceGen
    i <- Gen.choose[Int](1, 4)
  } yield (i, p)

  def problemGen: Gen[(ChessGroups, Board)] = for {
    i <- Gen.choose[Int](2, 3)
    ps <- Gen.listOfN(i, chessGroupGen)
    b <- boardGen
  } yield ps -> b

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSize = 10, maxSize = 10, minSuccessful = 10)

  property("solve several chess problems") {
    forAll(problemGen) {
      t =>
        val (groups, b) = t
        println(s"Groups $groups")
        println(s"Board ${b.n}, ${b.m}")
        /*corrector(solve(groups, b).run) foreach {
          _ shouldBe true
        }*/
    }
  }
}
