package com.bura.game

import com.bura.model.{Deck, Player}

object GameFactory {

  def create(humanName: String = "Human", robotName: String = "Robot"): Either[String, GameState] = {

    val deck = Deck.shuffled

    deck.topCard match {
      case Some(trumpCard) =>
        val human = Player.Human(name = humanName)
        val robot = Player.Robot(name = robotName)

        val initialState =
          GameState(deck = deck, players = Vector(human, robot), attacker = 0, defender = 1, trump = trumpCard.suit)

        Right(initialState.refillHands)

      case None => Left("Cannot create game: deck is empty")
    }
  }

  def createSession(humanName: String = "Human", robotName: String = "Robot"): Either[String, GameSession] =
    create(humanName = humanName, robotName = robotName).map(GameSession.apply)
}
