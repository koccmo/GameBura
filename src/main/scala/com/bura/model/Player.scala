package com.bura.model

import com.bura.rules.BuraRules
sealed trait Player {

  def name: String

  def hand: List[Card]

  def tricks: List[WonTrick.Open]

  def hiddenTricks: List[WonTrick.Hidden]

  // Hand operations
  def addCards(cards: List[Card]): Player

  def removeCards(cards: List[Card]): Player

  // Game actions
  def playAttack(attack: Attack): Player =
    removeCards(attack.cards)

  def playDefense(defense: Defense): Player =
    removeCards(defense.cards)

  // Tricks

  def addTrick(trick: WonTrick.Open): Player

  def addHiddenTrick(trick: WonTrick.Hidden): Player

  // Score

  lazy val trickPoints: Int =
    tricks
      .map(_.points)
      .sum

  lazy val totalPoints: Int =
    trickPoints +
      hiddenTricks
        .map(_.revealPoints)
        .sum

  // Helpers
  def hasCards(cards: List[Card]): Boolean = cards.forall(hand.contains)

  def needCards: Int = BuraRules.HandSize - hand.size

}

object Player {

  // Human player
  case class Human (name: String,
                    hand: List[Card] = Nil,
                    tricks: List[WonTrick.Open] = Nil,
                    hiddenTricks: List[WonTrick.Hidden] = Nil) extends Player {

    override def addCards(cards: List[Card]): Human =
      copy(hand = hand ++ cards)

    override def removeCards(cards: List[Card]): Human = {
      require(cards.forall(hand.contains), "Player doesn't have these cards")
      copy(hand = hand.diff(cards))}

    override def addTrick(trick: WonTrick.Open): Human =
      copy(tricks = tricks :+ trick)

    override def addHiddenTrick(trick: WonTrick.Hidden): Human =
      copy(hiddenTricks = hiddenTricks :+ trick)

  }

  // Robot player
  case class Robot(
                    name: String,
                    hand: List[Card] = Nil,
                    tricks: List[WonTrick.Open] = Nil,
                    hiddenTricks: List[WonTrick.Hidden] = Nil
                  ) extends Player {

    override def addCards(cards: List[Card]): Robot =
      copy(hand = hand ++ cards)

    override def removeCards(cards: List[Card]): Robot = {
      require(cards.forall(hand.contains), "Robot doesn't have these cards")
      copy(hand = hand.diff(cards))
    }

    override def addTrick(trick: WonTrick.Open): Robot =
      copy(tricks = tricks :+ trick)

    override def addHiddenTrick(trick: WonTrick.Hidden): Robot =
      copy(hiddenTricks = hiddenTricks :+ trick)

  }

}


