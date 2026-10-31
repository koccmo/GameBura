package com.bura.game

import com.bura.model.{Attack, Card, Deck, Defense, Player, Rank, Suit, WonTrick}
import org.scalatest.funsuite.AnyFunSuite

class GameSessionSpec extends AnyFunSuite {

  private val sixHearts = Card(Rank.Six, Suit.Hearts)
  private val tenClubs = Card(Rank.Ten, Suit.Clubs)
  private val aceHearts = Card (Rank.Ace, Suit.Hearts)
  private val tenSpades = Card(Rank.Ten, Suit.Spades)
  private val kingClubs = Card(Rank.King, Suit.Clubs)
  private val sevenHearts = Card(Rank.Seven, Suit.Hearts)


  test("applyAction applies attack and returns updated state") {

    val human = Player.Human(name = "Human", hand = List(sixHearts, tenClubs))
    val robot = Player.Robot(name = "Robot", hand = Nil)

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = Suit.Hearts
    )

    val session = GameSession(state)

    val attack = Attack(List(sixHearts))

    val result = session.applyAction(PlayerAction.AttackCards(attack))

    assert(result.isRight)

    result.toOption.get match {

      case SessionResult.Updated(newSession) =>
        assert(newSession.state.attackingPlayer.hand == List(tenClubs))
        assert(newSession.state.round == Round.WaitingDefense(attack))

      case SessionResult.Finished(_) => fail("Attack should update session, not finish game")
    }
  }

  test("applyAction returns error when action is invalid") {

    val human = Player.Human(name = "Human", hand = List(tenClubs))
    val robot = Player.Robot(name = "Robot", hand = Nil)

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = Suit.Hearts
    )

    val session = GameSession(state)

    val attack = Attack(List(sixHearts))

    val result = session.applyAction(PlayerAction.AttackCards(attack))

    assert(result == Left("Player does not have these cards"))
  }

  test("applyAction returns Finished when declaration ends the game") {

    val human = Player.Human(
      name = "Human",
      hand = Nil,
      tricks = List(WonTrick.Open(List(aceHearts, tenClubs, tenSpades)))
    )

    val robot = Player.Robot(
      name = "Robot",
      hand = Nil,
      tricks = List(WonTrick.Open(List(kingClubs)))
    )

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = Suit.Hearts
    )

    val session = GameSession(state)

    val result = session.applyAction(PlayerAction.Declare31)

    assert(result == Right(SessionResult.Finished(
            FinishedGame(state = state, result = GameResult.HumanWon(humanPoints = 31, robotPoints = 4))
          )
        )
    )
  }

  test("updated session can be used for the next action") {

    val human = Player.Human(name = "Human", hand = List(sixHearts))
    val robot = Player.Robot(name = "Robot", hand = List(sevenHearts))

    val state =
      GameState(
        deck = Deck(Nil),
        players = Vector(human, robot),
        attacker = 0,
        defender = 1,
        trump = Suit.Hearts
      )

    val session = GameSession(state)

    val attack = Attack(List(sixHearts))

    val afterAttack = session.applyAction(PlayerAction.AttackCards(attack))

    val updatedSession = afterAttack match {

        case Right(SessionResult.Updated(newSession)) => newSession
        case other => fail(s"Expected updated session, got: $other")
      }

    val defense = Defense(List(sevenHearts))

    val afterDefense = updatedSession.applyAction(PlayerAction.DefendCards(defense))

    assert(afterDefense.isRight)
  }

}
