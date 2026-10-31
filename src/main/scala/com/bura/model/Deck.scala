package com.bura.model

import scala.annotation.tailrec
import scala.util.Random

case class Deck(cards: List[Card]) {

  def drawOne: Option[(Card, Deck)] = {
    cards match {
      case Nil => None
      case head :: tail => Some(head, Deck(tail))
    }
  }

  def draw(count: Int): (List[Card], Deck) = {
    require(count >= 0, "Count cannot be negative")
    @tailrec
    def loop(deck: Deck, left: Int, cards: List[Card]): (List[Card], Deck) =
      if (left == 0) (cards.reverse, deck)
      else deck.drawOne match {
        case Some((card, newDeck)) => loop(newDeck, left - 1, card :: cards)
        case None => (cards.reverse, deck)
      }

    loop(this, count, Nil)
  }

  def drawTo(player: Player): (Player, Deck) = {
    val (cards, newDeck) =
      draw(player.needCards)

    (player.addCards(cards), newDeck)
  }

  def nonEmpty: Boolean = cards.nonEmpty
  def isEmpty: Boolean = cards.isEmpty
  def size: Int = cards.size
  def topCard: Option[Card] = cards.headOption
  def shuffle: Deck = copy(cards = util.Random.shuffle(cards))
}

object Deck {

  private def full: Deck =
    Deck(
      for {
        rank <- Rank.ValuesList
        suit <- Suit.ValuesList
      } yield Card(rank, suit)
    )

  def shuffled: Deck = full.shuffle
}
