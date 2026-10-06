package com.bura.rules

import com.bura.model.{Attack, Card, Defense, Suit}


object Rules {

  def beatsOne(attack: Card, defense: Card, trump: Suit): Boolean =
    if (defense.suit == attack.suit)
      defense.rank.strength > attack.rank.strength
    else
      defense.suit == trump

  def beatsAttack(a: Attack, d: Defense, trump: Suit): Boolean =
    if (a.cards.size != d.cards.size) false
    else
      d.cards.permutations.exists { defenseCards =>
        a.cards.zip(defenseCards).forall {
          case (ac, dc) => beatsOne(ac, dc, trump)
        }
      }
}
