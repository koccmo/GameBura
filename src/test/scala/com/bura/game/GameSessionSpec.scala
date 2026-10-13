package com.bura.game

import com.bura.model.{Attack, Card, Deck, Player, Rank, Suit}
import org.scalatest.funsuite.AnyFunSuite

class GameSessionSpec extends AnyFunSuite {

  private val sixHearts = Card(Rank.Six, Suit.Hearts)
  private val tenClubs = Card(Rank.Ten, Suit.Clubs)

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

      case ActionResult.StateUpdated(newState) =>
        assert(newState.attackingPlayer.hand == List(tenClubs))
        assert(newState.round == Round.WaitingDefense(attack))

      case ActionResult.GameFinished(_) => fail("Attack should update state, not finish game")
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

}
