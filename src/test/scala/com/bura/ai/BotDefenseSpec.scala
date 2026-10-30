package com.bura.ai

import com.bura.game.{GameState, Round}
import com.bura.model.{Attack, Card, Deck, Player, Rank, Suit}
import org.scalatest.funsuite.AnyFunSuite

class BotDefenseSpec extends AnyFunSuite {

  test("robot defends with a higher card of the same suit") {

    val attack = Attack(List(Card(rank = Rank.Seven, suit = Suit.Hearts)))

    val robot =
      Player.Robot(
        name = "Robot",
        hand =
          List(Card(rank = Rank.Eight, suit = Suit.Hearts),
          Card(rank = Rank.Six, suit = Suit.Clubs),
          Card(rank = Rank.Six, suit = Suit.Diamonds)),
        tricks = List.empty,
        hiddenTricks = List.empty
      )

    val human =
      Player.Human(
        name = "Human",
        hand = List.empty,
        tricks = List.empty,
        hiddenTricks = List.empty
      )

    val state =
      GameState(
        deck = Deck(List.empty),
        players = Vector(human, robot),
        attacker = 0,
        defender = 1,
        trump = Suit.Spades,
        round = Round.WaitingDefense(attack)
      )

    val result = BotDefense.choose(state = state, robot = robot, attack = attack)

    assert(result.isDefined)

    assert(result.get.cards == List(Card(rank = Rank.Eight, suit = Suit.Hearts)))
  }

  test("robot uses trump when it cannot beat with the same suit") {

    val attack =
      Attack(List(Card(rank = Rank.Seven, suit = Suit.Hearts)))

    val trumpCard =
      Card(rank = Rank.Six, suit = Suit.Spades)

    val robot =
      Player.Robot(
        name = "Robot",
        hand = List(Card(rank = Rank.Six, suit = Suit.Hearts), trumpCard, Card(rank = Rank.Six, suit = Suit.Clubs)),
        tricks = List.empty,
        hiddenTricks = List.empty
      )

    val human =
      Player.Human(
        name = "Human",
        hand = List.empty,
        tricks = List.empty,
        hiddenTricks = List.empty
      )

    val state =
      GameState(
        deck = Deck(List.empty),
        players = Vector(human, robot),
        attacker = 0,
        defender = 1,
        trump = Suit.Spades,
        round = Round.WaitingDefense(attack)
      )

    val result =
      BotDefense.choose(
        state = state,
        robot = robot,
        attack = attack
      )

    assert(result.isDefined)

    assert(result.get.cards == List(trumpCard))
  }

  test("robot returns None when it cannot defend") {

    val attack =
      Attack(
        List(
          Card(
            rank = Rank.Ace,
            suit = Suit.Hearts
          )
        )
      )

    val robot =
      Player.Robot(
        name = "Robot",
        hand = List(
          Card(rank = Rank.Six, suit = Suit.Hearts),
          Card(rank = Rank.Seven, suit = Suit.Clubs),
          Card(rank = Rank.Eight, suit = Suit.Diamonds)
        ),
        tricks = List.empty,
        hiddenTricks = List.empty
      )

    val human =
      Player.Human(
        name = "Human",
        hand = List.empty,
        tricks = List.empty,
        hiddenTricks = List.empty
      )

    val state =
      GameState(
        deck = Deck(List.empty),
        players = Vector(human, robot),
        attacker = 0,
        defender = 1,
        trump = Suit.Spades,
        round = Round.WaitingDefense(attack)
      )

    val result =
      BotDefense.choose(
        state = state,
        robot = robot,
        attack = attack
      )

    assert(result.isEmpty)
  }

  test("robot defends against two cards with two higher cards of the same suit") {

    val attack =
      Attack(
        List(
          Card(rank = Rank.Seven, suit = Suit.Hearts),
          Card(rank = Rank.Eight, suit = Suit.Hearts)
        )
      )

    val nineHearts = Card(rank = Rank.Nine, suit = Suit.Hearts)

    val tenHearts = Card(rank = Rank.Ten, suit = Suit.Hearts)
    val robot =
      Player.Robot(
        name = "Robot",
        hand = List(nineHearts, tenHearts, Card(rank = Rank.Six, suit = Suit.Clubs)),
        tricks = List.empty,
        hiddenTricks = List.empty
      )

    val human =
      Player.Human(
        name = "Human",
        hand = List.empty,
        tricks = List.empty,
        hiddenTricks = List.empty
      )

    val state =
      GameState(
        deck = Deck(List.empty),
        players = Vector(human, robot),
        attacker = 0,
        defender = 1,
        trump = Suit.Spades,
        round = Round.WaitingDefense(attack)
      )

    val result =
      BotDefense.choose(
        state = state,
        robot = robot,
        attack = attack
      )

    assert(result.isDefined)

    assert(result.get.cards.toSet == Set(nineHearts, tenHearts))
  }

  test("robot chooses one card when surrendering against one-card attack") {

    val attack =
      Attack(List(Card(rank = Rank.Ace, suit = Suit.Hearts)))

    val weakestCard = Card(rank = Rank.Six, suit = Suit.Clubs)

    val robot =
      Player.Robot(
        name = "Robot",
        hand = List(weakestCard,
          Card(rank = Rank.Ten, suit = Suit.Diamonds),
          Card(rank = Rank.King, suit = Suit.Hearts)
        ),
        tricks = List.empty,
        hiddenTricks = List.empty
      )

    val human =
      Player.Human(
        name = "Human",
        hand = List.empty,
        tricks = List.empty,
        hiddenTricks = List.empty
      )

    val state =
      GameState(
        deck = Deck(List.empty),
        players = Vector(human, robot),
        attacker = 0,
        defender = 1,
        trump = Suit.Spades,
        round = Round.WaitingDefense(attack)
      )

    val result =
      BotDefense.chooseSurrenderDefense(
        state = state,
        robot = robot,
        attack = attack
      )

    assert(result.isDefined)

    assert(result.get.cards == List(weakestCard))
  }
}