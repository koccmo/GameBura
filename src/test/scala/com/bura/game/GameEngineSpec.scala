package com.bura.game

import com.bura.model.Rank.{Ace, Ten}
import com.bura.model.Suit.{Clubs, Hearts}
import com.bura.model.{Card, Deck, Player, Rank, Suit, WonTrick}
import org.scalatest.funsuite.AnyFunSuite

class GameEngineSpec extends AnyFunSuite {

  private val tenClubs = Card(Ten, Clubs)
  private val aceHearts = Card(Ace, Hearts)
  private val tenHearts = Card(Rank.Ten, Suit.Hearts)
  private val kingSpared = Card(Rank.King, Suit.Spades)

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
    val result = GameEngine.play(state)

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
    val result = GameEngine.play(state)

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
    val result = GameEngine.play(state)

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

    val result = GameEngine.play(state)

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

    val result = GameEngine.play(state)

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

    val result = GameEngine.play(state)

    assert(result == Left("Robot player not found"))
  }

  

}
