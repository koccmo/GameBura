package com.bura.game

sealed trait SessionResult

object SessionResult {
  case class Updated(session : GameSession) extends SessionResult
  case class Finished(game: FinishedGame) extends SessionResult
}
