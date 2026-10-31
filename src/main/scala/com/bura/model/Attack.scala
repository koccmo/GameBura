package com.bura.model

import com.bura.rules.BuraRules

case class Attack(cards: List[Card]) {

  require(
    cards.nonEmpty &&
      cards.size <= BuraRules.MaxAttackCards,
    s"Attack must contain 1-${BuraRules.MaxAttackCards} cards"
  )

  private def sameSuit: Boolean = cards.map(_.suit).distinct.size == 1

  def size: Int = cards.size

  def suit: Suit = cards.head.suit

  def isValid: Boolean = sameSuit

  def points: Int = cards.map(_.points).sum
}
