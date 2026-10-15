package com.bura.game

final case class GameSessionStore(sessions: Map[Long, GameSession] = Map.empty) {

  def createGame(
                  id: Long,
                  humanName: String = "Human",
                  robotName: String = "Robot"): Either[String, GameSessionStore] =
    GameFactory
      .createSession(
        humanName = humanName,
        robotName = robotName
      )
      .map { session => put(id = id, session = session)
      }
  def get(id: Long): Option[GameSession] = sessions.get(id)

  def put(id: Long, session: GameSession): GameSessionStore = copy(sessions = sessions.updated(id, session))

  def remove(id: Long): GameSessionStore = copy(sessions = sessions.removed(id))


}