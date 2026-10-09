package com.bura.game

sealed trait ActionResult

object ActionResult {

  case class StateUpdated(state: GameState) extends ActionResult
  case class GameFinished(game: FinishedGame) extends ActionResult
}
