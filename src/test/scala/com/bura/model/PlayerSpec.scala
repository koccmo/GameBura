package com.bura.model

import com.bura.model.Rank.{Ace, Six, Ten}
import com.bura.model.Suit.{Clubs, Hearts, Spades}
import org.scalatest.funsuite.AnyFunSuite

class PlayerSpec extends AnyFunSuite {
  private val aceHearts: Card = Card(Ace, Hearts)
  private val tenClubs: Card = Card(Ten, Clubs)
  private val sixSpades: Card = Card(Six, Spades)
  private def createHuman(hand: List[Card] = Nil): Player.Human = Player.Human(name = "Human", hand = hand)

  test("addCards adds cards to player hand") {

    val human = createHuman()
    val cards = List(aceHearts, tenClubs)
    val updatedHuman = human.addCards(cards)

    assert(updatedHuman.hand == cards)
    assert(human.hand.isEmpty)
  }

  test("removeCards removes cards from player hand") {
    val human = createHuman(List(aceHearts, tenClubs))

    val updatedHuman = human.removeCards(List(aceHearts))

    assert(updatedHuman.hand == List(tenClubs))
    assert(human.hand == List(aceHearts, tenClubs))
  }

  test("removeCards throws exception when player does not have card") {

    val human = createHuman(List(aceHearts))

    assertThrows[IllegalArgumentException] { human.removeCards(List(tenClubs)) }
  }

  test("totalPoints includes hidden trick points") {

    val openTrick = WonTrick.Open(List(tenClubs))
    val hiddenTrick = WonTrick.Hidden(List(aceHearts))
    val human = Player.Human(name = "Human", tricks = List(openTrick), hiddenTricks = List(hiddenTrick))


    val visiblePoints = human.trickPoints
    val totalPoints = human.totalPoints

    assert(visiblePoints == 10)
    assert(totalPoints == 21)
  }

  test("needCards returns how many cards player needs to reach hand size") {

    val human = createHuman(List(aceHearts))
    val result = human.needCards

    assert(result == 2)

  }

  test("needCards returns zero when hand is full") {

    val human = createHuman(List(aceHearts, tenClubs, sixSpades))
    val result = human.needCards

    assert(result == 0)
  }

  test("addCards keeps existing cards and adds new cards") {

    val human = createHuman(List(aceHearts))
    val updatedHuman = human.addCards(List(tenClubs, sixSpades))

    assert(updatedHuman.hand == List(aceHearts, tenClubs, sixSpades))
  }

  test("removeCards removes multiple cards from player hand") {

    val human = createHuman(List(aceHearts, tenClubs, sixSpades))
    val updatedHuman = human.removeCards(List(aceHearts, sixSpades))

    assert(updatedHuman.hand == List(tenClubs))
  }

  test("needCards returns full hand size when hand is empty") {

    val human = createHuman()
    val result = human.needCards

    assert(result == 3)
  }
}
