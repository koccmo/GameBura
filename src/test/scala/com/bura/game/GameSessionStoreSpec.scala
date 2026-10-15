package com.bura.game

import com.bura.model.{Attack, Card, Deck, Player, Rank, Suit, WonTrick}
import org.scalatest.funsuite.AnyFunSuite


class GameSessionStoreSpec extends AnyFunSuite {

  private val sixHearts = Card(Rank.Six, Suit.Hearts)
  private val tenClubs = Card(Rank.Ten, Suit.Clubs)
  private val aceHearts = Card(Rank.Ace, Suit.Hearts)
  private val tenSpades = Card(Rank.Ten, Suit.Spades)
  private val kingClubs = Card(Rank.King, Suit.Clubs)

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

  test("createGame creates and stores new game session") {

    val store = GameSessionStore()

    val result = store.createGame(id = 1001L, humanName = "Roman")

    assert(result.isRight)

    val updatedStore = result.toOption.get

    val session = updatedStore.get(1001L)

    assert(session.isDefined)

    assert(session.get.state.players.exists {
        case human: Player.Human => human.name == "Roman"
        case _ => false
      }
    )
  }

  test("applyAction updates stored session when game continues") {

    val human = Player.Human(name = "Human", hand = List(sixHearts, tenClubs))

    val robot = Player.Robot(name = "Robot", hand = Nil)

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

    val attack = Attack(List(sixHearts))

    val result = store.applyAction(id = 1001L, action = PlayerAction.AttackCards(attack))

    assert(result.isRight)

    val (updatedStore, sessionResult) = result.toOption.get

    sessionResult match {

      case SessionResult.Updated(newSession) =>

        assert(newSession.state.attackingPlayer.hand == List(tenClubs))
        assert(updatedStore.get(1001L).contains(newSession))

      case SessionResult.Finished(_) => fail("Attack should update session, not finish game")
    }
  }

  test("applyAction removes session when game is finished") {

    val human =
      Player.Human(
        name = "Human",
        hand = Nil,
        tricks = List(WonTrick.Open(List(aceHearts , tenClubs, tenSpades)))
      )

    val robot =
      Player.Robot(
        name = "Robot",
        hand = Nil,
        tricks = List(WonTrick.Open(List(kingClubs)))
      )

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

    val result = store.applyAction(id = 1001L, action = PlayerAction.Declare31)

    assert(result.isRight)

    val (updatedStore, sessionResult) = result.toOption.get

    sessionResult match {

      case SessionResult.Finished(game) =>

        assert(game.result == GameResult.HumanWon(humanPoints = 31, robotPoints = 4))
        assert(updatedStore.get(1001L).isEmpty)

      case SessionResult.Updated(_) => fail("Declare31 should finish the game")
    }
  }

  test("applyAction returns error when session does not exist") {

    val store = GameSessionStore()

    val attack = Attack(List(sixHearts))

    val result = store.applyAction(id = 1001L, action = PlayerAction.AttackCards(attack))

    assert(result == Left("Game session not found"))
  }
  
}
