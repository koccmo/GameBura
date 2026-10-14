package com.bura.game

final case class GameSession (state: GameState) {

  def applyAction(action: PlayerAction): Either[String, SessionResult] =
    GameEngine
      .applyPlayerAction(state, action)
      .map {
        case ActionResult.StateUpdated(newState) => SessionResult.Updated(GameSession(newState))
        case ActionResult.GameFinished(game) => SessionResult.Finished(game)
      }
}
