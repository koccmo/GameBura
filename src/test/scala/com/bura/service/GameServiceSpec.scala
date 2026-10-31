package com.bura.service

import com.bura.game.{GameState, Round}
import com.bura.model.Rank.{Ace, Eight, King, Nine, Seven, Six, Ten}
import com.bura.model.Suit.{Clubs, Hearts, Spades}
import com.bura.model.{Attack, Card, Deck, Defense, Player, Rank, Suit, WonTrick}
import org.scalatest.funsuite.AnyFunSuite

class GameServiceSpec extends AnyFunSuite {

  private val trump = Suit.Hearts
  private val sixHearts = Card(Six, Hearts)
  private val sevenHearts = Card(Seven, Hearts)
  private val eightHearts = Card(Eight, Hearts)
  private val aceHearts = Card(Ace, Hearts)
  private val tenClubs = Card(Ten, Clubs)
  private val nineSpades = Card(Nine, Spades)
  private val eightSpades = Card(Eight, Spades)
  private val nineClubs = Card(Nine, Clubs)
  private val kingSpades = Card(King, Spades)


  test("attack removes cards from attacker hand and waits for defense") {


    val human = Player.Human(name = "Human", hand = List(sixHearts, tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(nineSpades))
    val state = GameState(deck = Deck(Nil), players = Vector(human, robot), attacker = 0, defender = 1, trump = trump)
    val attack = Attack(List(sixHearts))

    val result = GameService.attack(state, attack)

    assert(result.isRight)

    val newState = result.toOption.get

    assert(newState.attackingPlayer.hand == List(tenClubs))

    assert(newState.round == Round.WaitingDefense(attack))
  }

  test("attack with cards of different suits returns error") {

    val human = Player.Human(name = "Human", hand = List(sixHearts, tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(nineSpades))
    val state = GameState(deck = Deck(Nil), players = Vector(human, robot), attacker = 0, defender = 1, trump = trump)
    val attack = Attack(List(sixHearts, tenClubs))
    val result = GameService.attack(state, attack)

    assert(result.isLeft)
    assert(state.attackingPlayer.hand == List(sixHearts, tenClubs))
    assert(state.round == Round.WaitingAttack)
  }

  test("attack with card not in attacker hand returns error") {

    val human = Player.Human(name = "Human", hand = List(sixHearts))
    val robot = Player.Robot(name = "Robot", hand = List(nineSpades))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump
    )

    val attack = Attack(List(aceHearts))

    val result = GameService.attack(state, attack)

    assert(result.isLeft)

    assert(state.attackingPlayer.hand == List(sixHearts))
    assert(state.round == com.bura.game.Round.WaitingAttack)
  }

  test("attack returns error when game is waiting for defense") {

    val human = Player.Human(name = "Human", hand = List(sixHearts, tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(nineSpades))

    val firstAttack = Attack(List(sixHearts))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump,
      round = Round.WaitingDefense(firstAttack)
    )

    val secondAttack = Attack(List(tenClubs))

    val result = GameService.attack(state, secondAttack)

    assert(result.isLeft)
    assert(state.attackingPlayer.hand == List(sixHearts, tenClubs))
    assert(state.round == Round.WaitingDefense(firstAttack))
  }

  test("attack with multiple cards of same suit succeeds") {

    val human = Player.Human(name = "Human", hand = List(sixHearts, sevenHearts, tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(nineSpades))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump
    )

    val attack = Attack(List(sixHearts, sevenHearts))

    val result = GameService.attack(state, attack)

    assert(result.isRight)

    val newState = result.toOption.get

    assert(newState.attackingPlayer.hand == List(tenClubs))
    assert(newState.round == Round.WaitingDefense(attack))
  }

  test("defend succeeds when defense beats attack") {

    val human = Player.Human(name = "Human", hand = List(tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(sevenHearts, nineSpades))
    val attack = Attack(List(sixHearts))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump,
      round = Round.WaitingDefense(attack)
    )

    val defense = Defense(List(sevenHearts))

    val result = GameService.defend(state, defense)

    assert(result.isRight)

    val newState = result.toOption.get

    assert(!newState.players(1).hand.contains(sevenHearts))
  }

  test("defend returns error when defense has different number of cards than attack") {

    val human = Player.Human(name = "Human", hand = List(tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(sevenHearts, eightHearts, nineSpades))

    val attack = Attack(List(sixHearts, sevenHearts))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump,
      round = Round.WaitingDefense(attack)
    )

    val defense = Defense(List(eightHearts))

    val result = GameService.defend(state, defense)

    assert(result.isLeft)
    assert(state.players(1).hand == List(sevenHearts, eightHearts, nineSpades))
    assert(state.round == Round.WaitingDefense(attack))
  }

  test("defend returns error when defender does not have defense card") {

    val human = Player.Human(name = "Human", hand = List(tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(nineSpades))

    val attack = Attack(List(sixHearts))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump,
      round = Round.WaitingDefense(attack)
    )

    val defense = Defense(List(sevenHearts))

    val result = GameService.defend(state, defense)

    assert(result == Left("Player does not have these cards"))
    assert(state.players(1).hand == List(nineSpades))
    assert(state.round == Round.WaitingDefense(attack))
  }

  test("defend returns error when game is not waiting for defense") {

    val human = Player.Human(name = "Human", hand = List(tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(sevenHearts, nineSpades))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump,
      round = Round.WaitingAttack
    )

    val defense = Defense(List(sevenHearts))

    val result = GameService.defend(state, defense)

    assert(result == Left("There is no attack to defend"))
    assert(state.players(1).hand == List(sevenHearts, nineSpades))
    assert(state.round == Round.WaitingAttack)
  }

  test("defend succeeds but attacker wins when defense does not beat attack") {

    val human = Player.Human(name = "Human", hand = List(tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(sixHearts, nineSpades))

    val attack = Attack(List(sevenHearts))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump,
      round = Round.WaitingDefense(attack)
    )

    val defense = Defense(List(sixHearts))

    val result = GameService.defend(state, defense)

    assert(result.isRight)

    val newState = result.toOption.get

    assert(!newState.players(1).hand.contains(sixHearts))
  }

  test("surrender succeeds when defender places correct number of cards") {

    val human = Player.Human(name = "Human", hand = List(tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(sevenHearts, nineSpades))
    val attack = Attack(List(sixHearts))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump,
      round = Round.WaitingDefense(attack)
    )

    val defense = Defense(List(sevenHearts))

    val result = GameService.surrender(state, defense)

    assert(result.isRight)

    val newState = result.toOption.get

    assert(!newState.players(1).hand.contains(sevenHearts))
  }

  test("surrender returns error when defense has different number of cards than attack") {

    val human = Player.Human(name = "Human", hand = List(tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(sevenHearts, eightHearts, nineSpades))
    val attack = Attack(List(sixHearts, sevenHearts))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump,
      round = Round.WaitingDefense(attack)
    )

    val defense = Defense(List(eightHearts))
    val result = GameService.surrender(state, defense)

    assert(result == Left("You must place the same number of cards as the attack"))
    assert(state.players(1).hand == List(sevenHearts, eightHearts, nineSpades))
    assert(state.round == Round.WaitingDefense(attack))
  }

  test("surrender returns error when defender does not have defense card") {

    val human = Player.Human(name = "Human", hand = List(tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(nineSpades))
    val attack = Attack(List(sixHearts))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump,
      round = Round.WaitingDefense(attack)
    )

    val defense = Defense(List(sevenHearts))
    val result = GameService.surrender(state, defense)

    assert(result == Left("Player does not have these cards"))
    assert(state.players(1).hand == List(nineSpades))
    assert(state.round == Round.WaitingDefense(attack))
  }

  test("surrender returns error when game is not waiting for defense") {

    val human = Player.Human(name = "Human", hand = List(tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(sevenHearts, nineSpades))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump,
      round = Round.WaitingAttack
    )

    val defense = Defense(List(sevenHearts))
    val result = GameService.surrender(state, defense)

    assert(result == Left("There is no attack to surrender to"))
    assert(state.players(1).hand == List(sevenHearts, nineSpades))
    assert(state.round == Round.WaitingAttack)
  }

  test("surrender gives hidden trick to attacker") {

    val human = Player.Human(name = "Human", hand = List(tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(sevenHearts, nineSpades))

    val attack = Attack(List(sixHearts))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump,
      round = Round.WaitingDefense(attack)
    )

    val defense = Defense(List(sevenHearts))
    val result = GameService.surrender(state, defense)

    assert(result.isRight)

    val newState = result.toOption.get

    assert(
      newState.players(0).hiddenTricks ==
        List(WonTrick.Hidden(List(sevenHearts)))
    )

    assert(
      newState.players(0).tricks == List(WonTrick.Open(List(sixHearts)))
    )
  }

  test("defend finishes round and refills players hands") {

    val human = Player.Human(name = "Human", hand = List(sixHearts, tenClubs, aceHearts))
    val robot = Player.Robot(name = "Robot", hand = List(sevenHearts, nineSpades, eightSpades))

    val deck = Deck(List(eightHearts, nineClubs))

    // Game start
    val state = GameState(
      deck = deck,
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump
    )

    // Human attacked
    val attack = Attack(List(sixHearts))
    val afterAttack = GameService.attack(state, attack).toOption.get

    // Robot defense
    val defense = Defense(List(sevenHearts))
    val result = GameService.defend(afterAttack, defense)

    assert(result.isRight)

    val newState = result.toOption.get

    //Robot win and start next round
    assert(newState.attacker == 1)
    assert(newState.defender == 0)
    assert(newState.round == Round.WaitingAttack)
    // Robot get trick
    assert(newState.players(1).tricks == List(WonTrick.Open(List(sixHearts, sevenHearts))))
    //Robot take cards
    assert(newState.players(1).hand == List(nineSpades, eightSpades, eightHearts))
    // Human take cards
    assert(newState.players(0).hand == List(tenClubs, aceHearts, nineClubs))
    //Deck end
    assert(newState.deck.isEmpty)
  }

  test("refillHands distributes remaining cards alternately") {

    // Arrange
    val human = Player.Human(name = "Human", hand = List(sixHearts, sevenHearts, tenClubs))
    val robot = Player.Robot(name = "Robot", hand = List(eightHearts, aceHearts, nineSpades))

    val deck = Deck(List(nineClubs, kingSpades))

    val state = GameState(
      deck = deck,
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump
    )

    // Human attacks with two cards
    val attack = Attack(List(sixHearts, sevenHearts))

    val afterAttack = GameService.attack(state, attack).toOption.get

    // Robot defends and wins
    val defense = Defense(List(eightHearts, aceHearts))

    val result = GameService.defend(afterAttack, defense)

    // Assert
    assert(result.isRight)

    val newState = result.toOption.get

    // Robot is the winner
    assert(newState.attacker == 1)
    assert(newState.defender == 0)

    // Robot draws first
    assert(newState.players(1).hand == List(nineSpades, nineClubs))

    // Human draws second
    assert(newState.players(0).hand == List(tenClubs, kingSpades))

    assert(newState.deck.isEmpty)
    assert(newState.round == Round.WaitingAttack)
  }

  test("attacker wins round and draws cards first") {

    val human = Player.Human(name = "Human", hand = List(sevenHearts, tenClubs, aceHearts))
    val robot = Player.Robot(name = "Robot", hand = List(sixHearts, nineSpades, eightSpades))

    val state = GameState(
      deck = Deck(List(nineClubs, kingSpades)),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump
    )

    val attack = Attack(List(sevenHearts))

    val afterAttack = GameService.attack(state, attack).toOption.get

    val defense = Defense(List(sixHearts))

    val result = GameService.defend(afterAttack, defense)

    assert(result.isRight)

    val newState = result.toOption.get

    // Human win and attack again
    assert(newState.attacker == 0)
    assert(newState.defender == 1)
    assert(newState.round == Round.WaitingAttack)

    // Human get trick
    assert(newState.players(0).tricks.nonEmpty)

    // Human get cards from deck "first"
    assert(newState.players(0).hand == List(tenClubs, aceHearts, nineClubs))

    // Robot get cards from deck "second"
    assert(newState.players(1).hand == List(nineSpades, eightSpades, kingSpades))

    assert(newState.deck.isEmpty)
  }

  test("refillHands keeps hands unchanged when deck is empty") {

    val human = Player.Human(name = "Human", hand = List(sixHearts, tenClubs, aceHearts))
    val robot = Player.Robot(name = "Robot", hand = List(sevenHearts, nineSpades, eightSpades))

    val state = GameState(
      deck = Deck(Nil),
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump
    )

    val attack = Attack(List(sixHearts))

    val afterAttack = GameService.attack(state, attack).toOption.get

    val defense = Defense(List(sevenHearts))

    val result = GameService.defend(afterAttack, defense)

    assert(result.isRight)

    val newState = result.toOption.get

    // Robot wins
    assert(newState.attacker == 1)
    assert(newState.defender == 0)

    // Hands remain with two cards
    assert(newState.players(0).hand == List(tenClubs, aceHearts))

    assert(newState.players(1).hand == List(nineSpades, Card(Eight, Spades)))

    // No cards left
    assert(newState.deck.isEmpty)

    assert(newState.round == Round.WaitingAttack)
  }

  test("refillHands draws last trump card") {

    val human = Player.Human(name = "Human", hand = List(sixHearts, tenClubs, aceHearts))
    val robot = Player.Robot(name = "Robot", hand = List(sevenHearts, nineSpades, eightSpades))

    // Last card is trump
    val deck = Deck(List(nineClubs, eightHearts))

    val state = GameState(
      deck = deck,
      players = Vector(human, robot),
      attacker = 0,
      defender = 1,
      trump = trump
    )

    val attack = Attack(List(sixHearts))

    val afterAttack = GameService.attack(state, attack).toOption.get

    val defense = Defense(List(sevenHearts))

    val result = GameService.defend(afterAttack, defense)

    assert(result.isRight)

    val newState = result.toOption.get

    // Robot win and first get 9♣
    assert(newState.players(1).hand == List(nineSpades, Card(Eight, Spades), nineClubs))

    // Human get last card — 8♥
    assert(newState.players(0).hand == List(tenClubs, aceHearts, eightHearts))

    assert(newState.deck.isEmpty)
    assert(newState.trump == Hearts)
  }


}
