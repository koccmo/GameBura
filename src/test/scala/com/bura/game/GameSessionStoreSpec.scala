package com.bura.game

import com.bura.model.{Deck, Player, Suit}
import org.scalatest.funsuite.AnyFunSuite

class GameSessionStoreSpec extends AnyFunSuite {

  test("put stores session and get returns it") {

    val human = Player.Human(name = "Human")
    val robot = Player.Robot(name = "Robot")

    val state =
      GameState(
        deck = Deck(Nil),
        players = Vector(human, robot),
        attacker = 0,
        defender = 1,
        trump = Suit.Hearts
      )

    val session = GameSession(state)

    val store = GameSessionStore()

    val updatedStore = store.put(id = 1001L, session = session)

    assert(updatedStore.get(1001L).contains(session))
  }

  test("put does not change original store") {

    val human = Player.Human(name = "Human")
    val robot = Player.Robot(name = "Robot")

    val state =
      GameState(
        deck = Deck(Nil),
        players = Vector(human, robot),
        attacker = 0,
        defender = 1,
        trump = Suit.Hearts
      )

    val session = GameSession(state)

    val store = GameSessionStore()

    val updatedStore = store.put(id = 1001L, session = session)

    assert(store.get(1001L) == None)

    assert(updatedStore.get(1001L).contains(session))

  }

  test("remove deletes session from store") {

    val human = Player.Human(name = "Human")
    val robot = Player.Robot(name = "Robot")

    val state =
      GameState(
        deck = Deck(Nil),
        players = Vector(human, robot),
        attacker = 0,
        defender = 1,
        trump = Suit.Hearts
      )

    val session = GameSession(state)

    val store = GameSessionStore().put(id = 1001L, session = session)

    val updatedStore = store.remove(1001L)

    assert(store.get(1001L).contains(session))

    assert(updatedStore.get(1001L).isEmpty)
  }

  test("store keeps sessions separately by id") {

    val human1 = Player.Human(name = "Roman")
    val robot1 = Player.Robot(name = "Robot 1")

    val state1 =
      GameState(
        deck = Deck(Nil),
        players = Vector(human1, robot1),
        attacker = 0,
        defender = 1,
        trump = Suit.Hearts
      )

    val session1 = GameSession(state1)

    val human2 = Player.Human(name = "Alex")
    val robot2 = Player.Robot(name = "Robot 2")

    val state2 =
      GameState(
        deck = Deck(Nil),
        players = Vector(human2, robot2),
        attacker = 0,
        defender = 1,
        trump = Suit.Spades
      )

    val session2 = GameSession(state2)

    val store =
      GameSessionStore()
        .put(id = 1001L, session = session1)
        .put(id = 2002L, session = session2)


    assert(store.get(1001L).contains(session1))

    assert(store.get(2002L).contains(session2))
  }

}
