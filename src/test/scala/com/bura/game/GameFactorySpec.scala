package com.bura.game

import com.bura.model.Player
import org.scalatest.funsuite.AnyFunSuite

class GameFactorySpec extends AnyFunSuite {

  test("createSession creates game session with human and robot") {

    val result = GameFactory.createSession(humanName = "Roman", robotName = "Bot")

    assert(result.isRight)

    val session = result.toOption.get

    assert(session.state.players.exists {
        case human: Player.Human => human.name == "Roman"
        case _ => false
      }
    )

    assert(session.state.players.exists {
        case robot: Player.Robot => robot.name == "Bot"
        case _ => false
      }
    )
  }

  test("createSession deals 3 cards to each player") {

    val result = GameFactory.createSession()

    assert(result.isRight)

    val session = result.toOption.get

    assert(session.state.players.forall(_.hand.size == 3))
  }

  test("createSession starts with human as attacker and waiting for attack") {

    val result = GameFactory.createSession()

    assert(result.isRight)

    val session = result.toOption.get

    assert(session.state.attacker == 0)

    assert(session.state.defender == 1)

    assert(session.state.round == Round.WaitingAttack)
  }

}
