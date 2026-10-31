package com.bura.ai

import com.bura.game.GameState
import com.bura.model.{Card, Deck, Player, Rank, Suit}
import org.scalatest.funsuite.AnyFunSuite

class BotAttackSpec extends AnyFunSuite {

  private val sixSpades = Card(Rank.Six, Suit.Spades)
  private val sevenDiamonds = Card(Rank.Seven, Suit.Diamonds)
  private val eightSpades = Card(Rank.Eight, Suit.Spades)
  private val aceDiamonds = Card(Rank.Ace, Suit.Diamonds)
  private val sevenSpades = Card(Rank.Seven, Suit.Spades)
  private val tenClubs = Card(Rank.Ten, Suit.Clubs)
  private val sixClubs = Card(Rank.Six, Suit.Clubs)
  private val aceSpades = Card(Rank.Ace, Suit.Spades)
  private val sevenClubs = Card(Rank.Seven, Suit.Clubs)
  private val nineSpades = Card(Rank.Nine, Suit.Spades)
  private val sixDiamonds = Card(Rank.Six, Suit.Diamonds)
  private val aceClubs = Card(Rank.Ace, Suit.Clubs)

  test("robot attack should contain cards of the same suit") {

    val robot = Player.Robot(name = "Robot", hand = List(sixSpades, sevenDiamonds, eightSpades))
    val human = Player.Human(name = "Human")

    val state =
      GameState(
        deck = Deck(Nil),
        players = Vector(human, robot),
        attacker = 1,
        defender = 0,
        trump = Suit.Clubs
      )

    val result = BotAttack.choose(state = state, robot = robot)

    assert(result.isDefined)

    val attack = result.get

    assert(
      attack.cards
        .map(_.suit)
        .distinct
        .size == 1
    )
  }

  test("robot should be able to attack with two cards of the same suit") {

    val robot =
      Player.Robot(
        name = "Robot",
        hand = List(sixSpades, sevenSpades, aceDiamonds)
      )

    val human = Player.Human(name = "Human")

    val state =
      GameState(
        deck = Deck(Nil),
        players = Vector(human, robot),
        attacker = 1,
        defender = 0,
        trump = Suit.Clubs
      )

    val result = BotAttack.choose(state = state, robot = robot)

    assert(result.isDefined)

    val attack = result.get

    assert(attack.cards == List(sixSpades, sevenSpades))
  }

  test("robot should be able to attack with three cards of the same suit") {

    val robot = Player.Robot(name = "Robot", hand = List(sixSpades, sevenSpades, eightSpades))
    val human = Player.Human(name = "Human")

    val state =
      GameState(
        deck = Deck(Nil),
        players = Vector(human, robot),
        attacker = 1,
        defender = 0,
        trump = Suit.Clubs
      )

    val result = BotAttack.choose(state = state, robot = robot)

    assert(result.isDefined)

    val attack = result.get

    assert(attack.cards == List(sixSpades, sevenSpades, eightSpades))
  }

  test("robot should attack with two trump cards when trump points are high") {

    val robot =
      Player.Robot(
        name = "Robot",
        hand = List(tenClubs, sixClubs, aceSpades)
      )

    val human = Player.Human(name = "Human")

    val state =
      GameState(
        deck = Deck(Nil),
        players = Vector(human, robot),
        attacker = 1,
        defender = 0,
        trump = Suit.Clubs
      )

    val result = BotAttack.choose(state = state, robot = robot)

    assert(result.isDefined)

    val attack = result.get

    assert(attack.cards == List(tenClubs, sixClubs))
  }

  test("robot should save two low trump cards and attack with non-trump card") {

    val robot = Player.Robot(name = "Robot", hand = List(sixClubs, sevenClubs, eightSpades))
    val human = Player.Human(name = "Human")

    val state =
      GameState(
        deck = Deck(Nil),
        players = Vector(human, robot),
        attacker = 1,
        defender = 0,
        trump = Suit.Clubs
      )

    val result = BotAttack.choose(state = state, robot = robot)

    assert(result.isDefined)

    val attack = result.get

    assert(attack.cards == List(eightSpades))
  }

  test("robot should attack with two low trump cards when non-trump card has high points") {

    val robot = Player.Robot(name = "Robot", hand = List(sixClubs, sevenClubs, aceSpades))

    val human = Player.Human(name = "Human")

    val state =
      GameState(
        deck = Deck(Nil),
        players = Vector(human, robot),
        attacker = 1,
        defender = 0,
        trump = Suit.Clubs
      )

    val result = BotAttack.choose(state = state, robot = robot)

    assert(result.isDefined)

    val attack = result.get

    assert(attack.cards == List(sixClubs, sevenClubs))
  }

  test("robot should attack with the smallest card when there are no card combinations") {

    val robot = Player.Robot(name = "Robot", hand = List(nineSpades, sixDiamonds, aceClubs))

    val human = Player.Human(name = "Human")

    val state =
      GameState(
        deck = Deck(Nil),
        players = Vector(human, robot),
        attacker = 1,
        defender = 0,
        trump = Suit.Hearts
      )

    val result = BotAttack.choose(state = state, robot = robot)

    assert(result.isDefined)

    val attack = result.get

    assert(attack.cards == List(sixDiamonds))
  }

}
