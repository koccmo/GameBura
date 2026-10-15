package com.bura.game

final case class GameSessionStore(sessions: Map[Long, GameSession] = Map.empty) {

  def createGame(
    id: Long,
    humanName: String = "Human",
    robotName: String = "Robot"
  ): Either[String, GameSessionStore] =
    GameFactory
      .createSession(humanName = humanName, robotName = robotName)
      .map { session => put(id = id, session = session) }

  def applyAction(id: Long, action: PlayerAction): Either[String, (GameSessionStore, SessionResult)] =
    get(id)
      .toRight("Game session not found")
      .flatMap { session =>
        session.applyAction(action).map {

          case updated @ SessionResult.Updated(newSession) => (put(id, newSession), updated)
          case finished @ SessionResult.Finished(_)        => (remove(id), finished)
        }
      }

  def get(id: Long): Option[GameSession] = sessions.get(id)

  def put(id: Long, session: GameSession): GameSessionStore = copy(sessions = sessions.updated(id, session))

  def remove(id: Long): GameSessionStore = copy(sessions = sessions.removed(id))

}
