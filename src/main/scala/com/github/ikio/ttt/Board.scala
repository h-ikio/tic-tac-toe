package com.github.ikio.ttt

import scala.util.{Failure, Success, Try}

object Board {
  /**
   * 盤面の初期化。
   * @param size 正方形の盤面の一辺の長さ。
   * @return 初期化した盤面。
   */
	def apply(size: Int): Board = {
		// サイズ(正方形盤面の一辺のマス数)は1を超える奇数のみ受け付ける。
		require(size > 1)
		require(size % 2 == 1)

		val cells = {
			val height = size
			val width = size
			for {
				h <- 0 until height
			} yield {
				for {
					w <- 0 until width
				} yield {
					Cell(Position(w, h), Empty)
				}
			}
		}

		new Board(size, cells)
	}

  /**
   * 各セルの初期値の指定を伴う盤面の初期化。
   * @param cells 盤面のセルの集合。
   * @return 盤面。
   */
	def apply(cells: Seq[Seq[Cell]]): Board = {
		new Board(cells.size, cells)
	}

  // 勝敗判定。
	private def judge(board: Board, move: Move, cmp: (Cell, Move) => Boolean): Boolean = {
		board.cells.map {cs =>
			cs.filter {c =>
				cmp(c, move)
			}
		}.flatten.forall(c => c.state == move.state)
	}

	// Y軸方向の判定。
	def judgeVertical(board: Board, move: Move): Boolean = {
		// Note: Y軸方向の判定ということは、X座標が同じセルの比較。
		judge(board, move, (cell: Cell, move: Move) => cell.pos.x == move.pos.x)
	}

	// X軸方向の判定。
	def judgeHorizontal(board: Board, move: Move): Boolean = {
		// Note: X軸方向の判定ということは、Y座標が同じセルの比較。
		judge(board, move, (cell: Cell, move: Move) => cell.pos.y == move.pos.y)
	}

	// 斜め方向の判定。
	private def judgeDiagonal(board: Board, move: Move, line: Seq[Position]): Boolean = {
		board.cells.map {cs =>
			cs.filter {c =>
				line.contains(c.pos)
			}
		}.flatten.forall(c => c.state == move.state)
	}
	def judgeDiagonal(board: Board, move: Move): Boolean = {
		judgeDiagonal(board, move, board.diagonalLine1) || judgeDiagonal(board, move, board.diagonalLine2)
	}

	// 勝敗判定。
	def judge(board: Board, move: Move): Option[Decision] = {
		if (judgeVertical(board, move) || judgeHorizontal(board, move) || judgeDiagonal(board, move)) {
			move.state match {
				case Black => Option(BlackWin)
				case White => Option(WhiteWin)
				case _ => throw new RuntimeException("Something wrong.")
			}
		} else if (board.isFull) {
			// 盤面の空きがなく、かつ未決着ならドロー。
			Option(Draw)
		} else {
			None
		}
	}

	def process(board: Board, move: Move): Try[Board] = {
		board.process(move)
	}
}

/**
 * 正方形の盤面。
 * @param size 一辺の長さ。
 * @param cells 盤面のセルの集合。
 */
class Board private(val size: Int, val cells: Seq[Seq[Cell]]) {
	/**
	 * 盤面に空きがあるかどうか。
	 * @return 盤面に空きがなければtrue、そうでなければfalse。
	 */
	def isFull: Boolean = {
		cells.map {cs =>
			cs.exists(c =>
				c.state == Empty)
		}.forall(_ == false)
	}

	// 左上から右下にかけての斜めのラインの座標の集合。
	private val diagonalLine1: Seq[Position] =
		(for {
			h <- 0 until size
		} yield {
			for {
				w <- 0 until size
				if h == w
			} yield {
				Position(h, w)
			}
		}).flatten

	// 右上から左下にかけての斜めのラインの座標の集合。
	private val diagonalLine2: Seq[Position] =
		(for {
			h <- 0 until size
		} yield {
			for {
				w <- 0 until size
				if h + w == size - 1
			} yield {
				Position(h, w)
			}
		}).flatten

	/**
	 * 盤面の文字列表現。
	 * @return 盤面の文字列表現。先手(Black)のコマは "o"、後手(White)のコマは "x"、空きコマは "-" で表現される。
	 */
	override def toString: String = {
		cells.map {cs =>
			cs.map {c =>
				c.state.toString
			}.mkString(" ")
		}.mkString("\n") + "\n"
	}

	// 許容される手かどうかの判定。
	private def isAllowedMove(move: Move): Boolean = {
		val x = move.pos.x
		val y = move.pos.y

		Try {
			val cell = cells(y)(x)
			cell.state match {
				case Empty => true
				case _ => false // 空でないセルに対して手が指された。
			}
		} match {
			case Success(value) => value
			case Failure(_) => false // out of bounds
		}
	}

	// プレイヤーの指した手の処理。
	private def process(move: Move): Try[Board] = {
		Try {
			if (isAllowedMove(move)) {
				val nextCells =
					cells.map {cs =>
						cs.map {c =>
							if (c.pos == move.pos) {
								Cell(move.pos, move.state)
							} else {
								c
							}
						}
					}
				Board(nextCells)
			} else {
				// 許容されない座標の手が指された。
				throw new Exception("Invalid input!!")
			}
		}
	}
}
