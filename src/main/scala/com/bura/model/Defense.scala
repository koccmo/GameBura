package com.bura.model

import com.bura.rules.BuraRules


case class Defense(cards: List[Card]) {
  require(
    cards.nonEmpty &&
      cards.size <= BuraRules.MaxAttackCards,
    s"Defense must contain 1-${BuraRules.MaxAttackCards} cards"
  )

  def size: Int = cards.size

  def points: Int = cards.map(_.points).sum
}

