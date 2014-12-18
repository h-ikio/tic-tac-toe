package com.github.ikio.ttt

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Main extends App {
	println("Initializing...")

	val DEFAULT_BOARD_SIZE = 3

	val board =
    if (args.size == 1) {
      Board(args(0).toInt)
    } else {
      Board(DEFAULT_BOARD_SIZE)
    }

	println("Initialized.\n")

	println(board)

	process(B, board)

	@tailrec
	def process(player: Player, board: Board): Unit = {
		val (move, newBoard) = doProcess(player, board)
		newBoard match {
			case Success(nextBoard) =>
				// 一手指した後の盤面の表示。
				println(nextBoard)

				if (judge(player, nextBoard, move)) {
					// 決着が付いた。
					println("End.")
				} else {
					// まだ決着は付いていないので次のプレイヤーの手番。
					val nextPlayer = player match {
						case B => W
						case W => B
					}
					process(nextPlayer, nextBoard)
				}
			case Failure(e) =>
				// 不正な手なので同じプレイヤーに指し直させる。
				println(e.getMessage + " Please re-input coordinate.")
				println(board)
				process(player, board)
		}
	}

	@tailrec
	private def acceptInput(state: State): Move = {
		Try {
			val scanner = new java.util.Scanner(System.in)
			val x, y = scanner.nextInt()
			(x, y)
		} match {
			case Success(pos) =>
				// 座標として扱うことのできる入力が行われたため処理する。
				Move(Position(pos._1, pos._2), state)
			case Failure(_) =>
				// 座標として扱うことができない入力値であるため即座に再入力を求める。
				println(" Please re-input coordinate.¥n")
				acceptInput(state)
		}
	}

	private def doProcess(player: Player, board: Board): (Move, Try[Board]) = {
		val state =
			player match {
				case B =>
					println("Black's turn.")
					Black
				case W =>
					println("White's turn.")
					White
			}
		println("Please input coordinate: ")

		val move = acceptInput(state)

    println("")
    (move, Board.process(board, move))
	}

	def judge(player: Player, board: Board, move: Move): Boolean = {
		Board.judge(board, move: Move).map {result =>
			println(result)
			true
		} getOrElse {
			false
		}
	}
}
