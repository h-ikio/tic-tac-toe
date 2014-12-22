package com.github.ikio.ttt

import org.scalatest.{FunSpec, Matchers}

import scala.annotation.tailrec

class BoardTest extends FunSpec with Matchers {
	// 盤面に対して手のセットを再帰的に処理する。
	@tailrec
	private def processMovesRecursively(board: Board, moves: Seq[Move]): Board = {
		// Note: テスト用メソッドであるため、不正な手によるFailure返り値の可能性を無視している。
		moves match {
			case (head :: Nil) => Board.process(board, head).get
			case (head :: tail) => processMovesRecursively(Board.process(board, head).get, tail)
			case _ => board
		}
	}

	describe("A board") {

		describe("when empty") {
			it("should be judged to None") {
				Board.judge(Board(3), Move(Position(0, 0), Black)) should be (None)
			}
		}

		describe("when black places three respective marks in a horizontal") {
			it("should be judged to BlackWin") {
				val moves =
					Seq(
						Move(Position(0, 0), Black),
						Move(Position(0, 1), White),
						Move(Position(1, 0), Black),
						Move(Position(0, 2), White),
						Move(Position(2, 0), Black)
					)

				val lastMove = moves.last
				val board = processMovesRecursively(Board(3), moves)

				println(board)

				Board.judge(board, lastMove) should be (Some(BlackWin))
				Board.judgeHorizontal(board, lastMove) shouldBe true
				Board.judgeVertical(board, lastMove) shouldBe false
				Board.judgeDiagonal(board, lastMove) shouldBe false
			}
		}

		describe("when white places three respective marks in a vertical") {
			it("should be judged to WhiteWin") {
				val moves =
					Seq(
						Move(Position(1, 2), Black),
						Move(Position(0, 0), White),
						Move(Position(1, 0), Black),
						Move(Position(0, 1), White),
						Move(Position(2, 2), Black),
						Move(Position(0, 2), White)
					)

				val lastMove = moves.last
				val board = processMovesRecursively(Board(3), moves)

				println(board)

				Board.judge(board, lastMove) should be (Some(WhiteWin))
				Board.judgeHorizontal(board, lastMove) shouldBe false
				Board.judgeVertical(board, lastMove) shouldBe true
				Board.judgeDiagonal(board, lastMove) shouldBe false
			}
		}

		describe("when black places three respective marks in a diagonal(left upper to right lower)") {
			it("should be judged to BlackWin") {
				val moves =
					Seq(
						Move(Position(0, 0), Black),
						Move(Position(1, 0), White),
						Move(Position(1, 1), Black),
						Move(Position(0, 1), White),
						Move(Position(2, 2), Black)
					)

				val lastMove = moves.last
				val board = processMovesRecursively(Board(3), moves)

				println(board)

				Board.judge(board, lastMove) should be (Some(BlackWin))
				Board.judgeHorizontal(board, lastMove) shouldBe false
				Board.judgeVertical(board, lastMove) shouldBe false
				Board.judgeDiagonal(board, lastMove) shouldBe true
			}
		}

		describe("when white places three respective marks in a diagonal(right upper to left lower)") {
			it("should be judged to WhiteWin") {
				val moves =
					Seq(
						Move(Position(0, 0), Black),
						Move(Position(2, 0), White),
						Move(Position(0, 1), Black),
						Move(Position(1, 1), White),
						Move(Position(2, 2), Black),
						Move(Position(0, 2), White)
					)

				val lastMove = moves.last
				val board = processMovesRecursively(Board(3), moves)

				println(board)

				Board.judge(board, lastMove) should be (Some(WhiteWin))
				Board.judgeHorizontal(board, lastMove) shouldBe false
				Board.judgeVertical(board, lastMove) shouldBe false
				Board.judgeDiagonal(board, lastMove) shouldBe true
			}
		}

		describe("when full and black and white do not places three respective marks in any line") {
			it("should be judged to Draw") {
				val moves =
					Seq(
						Move(Position(0, 0), Black),
						Move(Position(0, 1), White),
						Move(Position(0, 2), Black),
						Move(Position(1, 1), White),
						Move(Position(1, 0), Black),
						Move(Position(1, 2), White),
						Move(Position(2, 1), Black),
						Move(Position(2, 0), White),
						Move(Position(2, 2), Black)
					)

				val lastMove = moves.last
				val board = processMovesRecursively(Board(3), moves)

				println(board)

				Board.judge(board, lastMove) should be (Some(Draw))
				Board.judgeHorizontal(board, lastMove) shouldBe false
				Board.judgeVertical(board, lastMove) shouldBe false
				Board.judgeDiagonal(board, lastMove) shouldBe false
			}
		}

	}
}