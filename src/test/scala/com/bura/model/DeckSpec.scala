package com.bura.model

import com.bura.model.Rank.{Ace, Nine, Six, Ten}
import com.bura.model.Suit.{Clubs, Hearts}
import org.scalatest.funsuite.AnyFunSuite

class DeckSpec extends AnyFunSuite{

  private val aceHearts: Card = Card(Ace, Hearts)
  private val tenClubs: Card = Card(Ten, Clubs)
  private val sixClubs: Card = Card(Six, Clubs)
  private val nineDiamonds = Card(Nine, Suit.Diamonds)

  test("drawOne returns first card and deck without that card") {

    val deck = Deck(List(aceHearts, tenClubs))
    val result = deck.drawOne

    assert(result.contains((aceHearts, Deck(List(tenClubs)))))
  }

  test("drawOne returns None when deck is empty") {

    val deck = Deck(Nil)
    val result = deck.drawOne

    assert(result.isEmpty)
  }

  test("draw returns requested cards and remaining deck") {

    val deck = Deck(List(aceHearts, tenClubs, sixClubs))
    val (drawnCards, newDeck) = deck.draw(2)

    assert(drawnCards == List(aceHearts, tenClubs))

    assert(newDeck == Deck(List(sixClubs)))
  }

  test("draw returns remaining cards when fewer cards are available") {

    val deck = Deck(List(aceHearts, tenClubs))
    val (drawnCards, newDeck) = deck.draw(3)

    assert(drawnCards == List(aceHearts, tenClubs))

    assert(newDeck.isEmpty)
  }

  test("draw zero cards returns empty list and unchanged deck") {

    val deck = Deck(List(aceHearts, tenClubs))
    val (drawnCards, newDeck) = deck.draw(0)

    assert(drawnCards.isEmpty)
    assert(newDeck == deck)
  }

  test("drawTo fills player hand up to hand size") {

    val human = Player.Human(name = "Human", hand = List(nineDiamonds))
    val deck = Deck(List(aceHearts, tenClubs, sixClubs))
    val (updatedPlayer, newDeck) = deck.drawTo(human)

    assert(updatedPlayer.hand == List(nineDiamonds, aceHearts, tenClubs))
    assert(newDeck == Deck(List(sixClubs)))
  }

  test("drawTo does not draw cards when player hand is already full") {

    val human = Player.Human(name = "Human", hand = List(aceHearts, tenClubs, sixClubs))
    val deck = Deck(List(nineDiamonds))
    val (updatedPlayer, newDeck) = deck.drawTo(human)

    assert(updatedPlayer.hand == List(aceHearts, tenClubs, sixClubs))
    assert(newDeck == deck)
  }

  test("drawTo gives remaining cards when deck has fewer cards than player needs"){
    val human = Player.Human(name = "Human", hand = Nil)
    val deck = Deck(List(aceHearts, tenClubs))
    val (updatedPlayer, newDeck) = deck.drawTo(human)

    assert(updatedPlayer.hand == List(aceHearts, tenClubs))
    assert(newDeck.isEmpty)
  }

  test("drawTo keeps existing cards and adds only cards needed to fill hand") {

    val human = Player.Human(name = "Human", hand = List(nineDiamonds, aceHearts))
    val deck = Deck(List(tenClubs, sixClubs))
    val (updatedPlayer, newDeck) = deck.drawTo(human)

    assert(updatedPlayer.hand == List(nineDiamonds, aceHearts, tenClubs))
    assert(newDeck == Deck(List(sixClubs)))
  }
}
