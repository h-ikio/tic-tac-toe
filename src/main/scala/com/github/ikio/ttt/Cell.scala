package com.github.ikio.ttt

/**
 * プレイヤーの指す手。
 * @param pos 指した手の座標。
 * @param state 指した後のセルの状態。
 */
case class Move(pos: Position, state: State) {
	// 指した後の状態はBlackかWhiteになるはず。
	require(state == White || state == Black)
}

/**
 * 座標。原点は左上。
 * @param x x方向の座標。
 * @param y y方向の座標。
 */
case class Position(x: Int, y: Int)

/**
 * セルの状態。
 */
trait State
case object Empty extends State {
	override def toString: String = "-"
}
case object Black extends State {
	override def toString: String = "o"
}
case object White extends State {
	override def toString: String = "x"
}

/**
 * セル。
 * @param pos セルの座標。
 * @param state セルの状態。
 */
case class Cell(pos: Position, state: State)

/**
 * プレイヤー。
 */
trait Player { def state: State }
// 先手
case object B extends Player {
	override def state: State = Black
}
// 後手
case object W extends Player {
	override def state: State = White
}

/**
 * 勝敗。
 */
trait Decision
case object BlackWin extends Decision {
	override def toString: String = {
		"Winner: Black!"
	}
}
case object WhiteWin extends Decision {
	override def toString: String = {
		"Winner: White!"
	}
}
case object Draw extends Decision {
	override def toString: String = {
		"Draw."
	}
}