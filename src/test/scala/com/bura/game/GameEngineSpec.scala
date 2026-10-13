package com.bura.game

import com.bura.model.Rank.{Ace, Ten}
import com.bura.model.Suit.{Clubs, Hearts}
import com.bura.model.{Attack, Card, Deck, Defense, Player, Rank, Suit, WonTrick}
import com.bura.ui.ConsoleGameRunner
import org.scalatest.funsuite.AnyFunSuite

class GameEngineSpec extends AnyFunSuite {

  private val tenClubs = Card(Ten, Clubs)
  private val aceHearts = Card(Ace, Hearts)
  private val tenHearts = Card(Rank.Ten, Suit.Hearts)
  private val kingSpared = Card(Rank.King, Suit.Spades)
  private val tenSpades = Card(Rank.Ten, Suit.Spades)
  private val kingClubs = Card(Rank.King, Suit.Clubs)
  private val sixHearts = Card(Rank.Six, Suit.Hearts)
  private val nineSpared = Card(Rank.Nine, Suit.Spades)
  private val sevenHearts = Card(Rank.Seven, Suit.Hearts)
  private val nineSpades = Card(Rank.Nine, Suit.Spades)
  private val sevenClubs = Card(Rank.Seven, Suit.Clubs)

  test("play returns HumanWon when human has more points") {

    // Arrange
    val human = Player.Human(
      name = "Human",
      hand = Nil,
      tricks = List(
        WonTrick.Open(List(aceHearts))
      )
    )

    val robot = Player.Robot(
      name = "Robot",
      hand = Nil,
      tricks = List(
        WonTrick.Open(List(tenClubs))
      )
    )

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = Suit.Hearts
    )

    // Act
    val result = ConsoleGameRunner.play(state)

    // Assert
    assert(result.isRight)

    val finishedGame = result.toOption.get

    assert(finishedGame.state == state)

    assert(finishedGame.result == GameResult.HumanWon(humanPoints = 11, robotPoints = 10))
  }

  test("play returns RobotWon when robot has more points") {

    // Arrange
    val human = Player.Human(name = "Human", hand = Nil, tricks = List(WonTrick.Open(List(tenClubs))))

    val robot = Player.Robot(name = "Robot", hand = Nil, tricks = List(WonTrick.Open(List(aceHearts))))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = Suit.Hearts
    )

    // Act
    val result = ConsoleGameRunner.play(state)

    // Assert
    assert(result.isRight)

    val finishedGame = result.toOption.get

    assert(finishedGame.state == state)

    assert(finishedGame.result == GameResult.RobotWon(humanPoints = 10, robotPoints = 11))
  }

  test("play returns Draw when both players have equal points") {

    // Arrange
    val human = Player.Human(name = "Human", hand = Nil, tricks = List(WonTrick.Open(List(tenClubs))))
    val robot = Player.Robot(name = "Robot", hand = Nil, tricks = List(WonTrick.Open(List(tenHearts))))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = Suit.Hearts
    )

    // Act
    val result = ConsoleGameRunner.play(state)

    // Assert
    assert(result.isRight)

    val finishedGame = result.toOption.get

    assert(finishedGame.state == state)

    assert(finishedGame.result == GameResult.Draw(humanPoints = 10, robotPoints = 10))
  }

  test("play includes hidden trick points in final result") {

    val human = Player.Human(
      name = "Human",
      hand = Nil,
      tricks = List(WonTrick.Open(List(tenClubs))),
      hiddenTricks = List(WonTrick.Hidden(List(aceHearts)))
    )

    val robot = Player.Robot(
      name = "Robot",
      hand = Nil,
      tricks = List(WonTrick.Open(List(kingSpared)))
    )

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = Suit.Hearts
    )

    val result = ConsoleGameRunner.play(state)

    assert(result.isRight)

    val finishedGame = result.toOption.get

    assert(finishedGame.result == GameResult.HumanWon(humanPoints = 21, robotPoints = 4))
  }

  test("play returns error when human player is missing") {

    val robot = Player.Robot(
      name = "Robot",
      hand = Nil,
      tricks = List(WonTrick.Open(List(Card(Rank.Ace, Suit.Hearts)))))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(robot),
      attacker = 0,
      defender = 0,
      trump = Suit.Hearts
    )

    val result = ConsoleGameRunner.play(state)

    assert(result == Left("Human player not found"))
  }

  test("play returns error when robot player is missing") {

    val human = Player.Human(
      name = "Human",
      hand = Nil,
      tricks = List(
        WonTrick.Open(List(Card(Rank.Ace, Suit.Hearts)))
      )
    )

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human),
      attacker = 0,
      defender = 0,
      trump = Suit.Hearts
    )

    val result = ConsoleGameRunner.play(state)

    assert(result == Left("Robot player not found"))
  }

  test("Declare31 finishes game with HumanWon when human has 31 points") {

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

    val result = GameEngine.applyPlayerAction(state, PlayerAction.Declare31)

    assert(result == Right(ActionResult.GameFinished(
      FinishedGame(state = state, result = GameResult.HumanWon(humanPoints = 31, robotPoints = 4))
          )
        )
    )
  }

  test("Declare31 finishes game with RobotWon when human has less than 31 points") {

    val human = Player.Human(
      name = "Human",
      hand = Nil,
      tricks = List(WonTrick.Open(List(tenHearts, tenClubs, tenSpades)))
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

    val result = GameEngine.applyPlayerAction(state, PlayerAction.Declare31)

    assert(result == Right(ActionResult.GameFinished(
      FinishedGame(state = state, result = GameResult.RobotWon(humanPoints = 30, robotPoints = 4))
          )
        )
    )
  }

  test("Declare31 finishes game with RobotWon when robot has 31 points") {

    val human = Player.Human(
      name = "Human",
      hand = Nil,
      tricks = List(WonTrick.Open(List(kingClubs)))
    )

    val robot = Player.Robot(
      name = "Robot",
      hand = Nil,
      tricks = List(WonTrick.Open(List(aceHearts, tenClubs, tenSpades)))
    )

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 1,
      defender = 0,
      trump = Suit.Hearts
    )

    val result = GameEngine.applyPlayerAction(state, PlayerAction.Declare31)

    assert(result == Right(ActionResult.GameFinished(
            FinishedGame(
              state = state,
              result = GameResult.RobotWon(humanPoints = 4, robotPoints = 31)
            )
          )
        )
    )
  }

  test("Declare31 finishes game with HumanWon when robot has less than 31 points") {

    val human = Player.Human(
      name = "Human",
      hand = Nil,
      tricks = List(WonTrick.Open(List(kingClubs)))
    )

    val robot = Player.Robot(
      name = "Robot",
      hand = Nil,
      tricks = List(WonTrick.Open(List(tenHearts, tenClubs, tenSpades)))
    )

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 1,
      defender = 0,
      trump = Suit.Hearts
    )

    val result = GameEngine.applyPlayerAction(state, PlayerAction.Declare31)

    assert(result == Right(ActionResult.GameFinished(
            FinishedGame(
              state = state,
              result = GameResult.HumanWon(humanPoints = 4, robotPoints = 30)
            )
          )
        )
    )
  }

  test("Continue returns StateUpdated with unchanged state") {

    val human = Player.Human(name = "Human", hand = Nil)
    val robot = Player.Robot(name = "Robot", hand = Nil)
    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = Suit.Hearts
    )

    val result = GameEngine.applyPlayerAction(state, PlayerAction.Continue)

    assert(result == Right(ActionResult.StateUpdated(state)))
  }

  test("AttackCards updates state through GameService") {

    val human = Player.Human(name = "Human", hand = List(sixHearts, tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(nineSpared))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = Suit.Hearts
    )

    val attack = Attack(List(sixHearts))

    val result = GameEngine.applyPlayerAction(state, PlayerAction.AttackCards(attack))

    assert(result.isRight)

    val actionResult = result.toOption.get

    actionResult match {

      case ActionResult.StateUpdated(newState) =>
        assert(newState.attackingPlayer.hand == List(tenClubs))
        assert(newState.round == Round.WaitingDefense(attack))

      case ActionResult.GameFinished(_) => fail("Attack should update state, not finish game")
    }
  }

  test("DefendCards updates state through GameService") {

    val human = Player.Human(name = "Human", hand = List(tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(sevenHearts, nineSpades))

    val attack = Attack(List(sixHearts))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = Suit.Hearts,
      round = Round.WaitingDefense(attack)
    )

    val defense = Defense(List(sevenHearts))

    val result = GameEngine.applyPlayerAction(state, PlayerAction.DefendCards(defense))

    assert(result.isRight)

    result.toOption.get match {

      case ActionResult.StateUpdated(newState) =>

        assert(!newState.players(1).hand.contains(sevenHearts))

        assert(newState.attacker == 1)

        assert(newState.round == Round.WaitingAttack)

      case ActionResult.GameFinished(_) =>
        fail("Defense should update state, not finish game")
    }
  }

  test("SurrenderCards updates state through GameService") {

    val human = Player.Human(name = "Human", hand = List(tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(sevenHearts, nineSpades))
    val attack = Attack(List(sixHearts))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = Suit.Hearts,
      round = Round.WaitingDefense(attack)
    )

    val defense = Defense(List(sevenHearts))

    val result = GameEngine.applyPlayerAction(state, PlayerAction.SurrenderCards(defense))
    assert(result.isRight)

    result.toOption.get match {
      case ActionResult.StateUpdated(newState) =>
        assert(!newState.players(1).hand.contains(sevenHearts))
        assert(newState.players(0).tricks == List(WonTrick.Open(List(sixHearts))))
        assert(newState.players(0).hiddenTricks == List(WonTrick.Hidden(List(sevenHearts))))
        assert(newState.attacker == 0)
        assert(newState.round == Round.WaitingAttack)

      case ActionResult.GameFinished(_) =>
        fail("Surrender should update state, not finish game")
    }
  }

  test("playRobotAttack applies robot attack and updates state") {

    val human = Player.Human(name = "Human", hand = List(tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(sixHearts))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 1,
      defender = 0,
      trump = Suit.Hearts
    )

    val result = GameEngine.playRobotAttack(state, robot)

    assert(result.isRight)

    val newState = result.toOption.get

    assert(newState.players(1).hand.isEmpty)

    assert(newState.round == Round.WaitingDefense(Attack(List(sixHearts))))
  }

  test("playRobotDefense applies robot defense and updates state") {

    val human = Player.Human(name = "Human", hand = List(tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(sevenHearts))
    val attack = Attack(List(sixHearts))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = Suit.Hearts,
      round = Round.WaitingDefense(attack)
    )

    val result = GameEngine.playRobotDefense(state, robot, attack)

    assert(result.isRight)

    val newState = result.toOption.get

    assert(newState.players(1).hand.isEmpty)

    assert(newState.attacker == 1)

    assert(newState.round == Round.WaitingAttack)
  }

  test("playRobotSurrender applies surrender and updates state") {

    val human = Player.Human(name = "Human", hand = List(tenSpades))
    val robot = Player.Robot(name = "Robot", hand = List(sevenClubs))
    val attack = Attack(List(sixHearts))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = Suit.Hearts,
      round = Round.WaitingDefense(attack)
    )

    val result = GameEngine.playRobotSurrender(state, robot, attack)

    assert(result.isRight)

    val newState = result.toOption.get

    assert(newState.players(1).hand.isEmpty)

    assert(newState.players(0).tricks == List(WonTrick.Open(List(sixHearts))))

    assert(newState.players(0).hiddenTricks == List(WonTrick.Hidden(List(sevenClubs))))

    assert(newState.attacker == 0)

    assert(newState.round == Round.WaitingAttack)
  }

}
