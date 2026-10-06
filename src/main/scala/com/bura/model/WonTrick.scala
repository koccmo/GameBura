package com.bura.model

sealed trait WonTrick {
  def count: Int
}

object WonTrick {

  case class Open(cards: List[Card]) extends WonTrick {

    lazy val points: Int = cards.map(_.points).sum

    override def count: Int = cards.size
  }

  case class Hidden(cards: List[Card]) extends WonTrick {

    override def count: Int = cards.size

    def reveal: List[Card] = cards

    def revealPoints: Int = cards.map(_.points).sum
  }
}
