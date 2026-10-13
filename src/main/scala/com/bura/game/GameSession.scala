package com.bura.game

final case class GameSession (state: GameState) {

  def applyAction(action: PlayerAction): Either[String, ActionResult] =
    GameEngine.applyPlayerAction(state, action)
}
