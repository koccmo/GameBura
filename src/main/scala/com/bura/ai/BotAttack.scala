package com.bura.ai

import com.bura.game.GameState
import com.bura.model.{Attack, Card, Player}

import scala.util.Try

object BotAttack {

  def choose(state: GameState, robot: Player.Robot): Option[Attack] = {

    val analysis: HandAnalysis = HandAnalysis.from(robot.hand, state.trump)
    val attack: List[Attack] = possibleAttacks(robot.hand)

    chooseAttack(attack, analysis)
  }

  private def possibleAttacks(hand: List[Card]): List[Attack] = {

    (1 to math.min(3, hand.size))
      .iterator
      .flatMap(hand.combinations)
      .flatMap { cards => Try(Attack(cards)).toOption.filter(_.isValid) }
      .toList
  }

  private def chooseAttack(attacks: List[Attack], analysis: HandAnalysis): Option[Attack] = {

    if (attacks.isEmpty) {
      None
    } else {

      val twoTrumps = analysis.trumpCards.size == 2
      val twoSuits = analysis.cards.groupBy(_.suit).values.exists(_.size >= 2)

      if (twoTrumps) {
        chooseTwoTrumpAttack(attacks,analysis)
      } else if (twoSuits) {
        chooseTwoSuitsAttack(attacks, analysis)
      } else {
        chooseSmallestAttack(attacks, analysis)
      }
    }
  }

  private def chooseTwoTrumpAttack(attacks: List[Attack], analysis: HandAnalysis): Option[Attack] = {

    val trumpPoints = analysis.trumpPoints
    val otherPoints = analysis.nonTrumpPoints
    val preferredCards =
      if (trumpPoints == 21) {
        analysis.trumpCards
      } else if (trumpPoints >= 10) {
        analysis.trumpCards
      } else if (trumpPoints <= 4 && otherPoints >= 10) {
        analysis.trumpCards
      } else {
        analysis.nonTrumpCards
      }

    findBestAttackContaining(attacks, preferredCards)
  }

  private def chooseTwoSuitsAttack(attacks: List[Attack], analysis: HandAnalysis): Option[Attack] = {

    val twoCardSuit = analysis
      .cards
      .groupBy(_.suit)
      .values
      .find(_.size >= 2)

    twoCardSuit match {
      case Some(cards) => findBestAttackContaining(attacks, cards)
      case None => chooseSmallestAttack(attacks, analysis)
    }
  }

  private def chooseSmallestAttack(attacks: List[Attack], analysis: HandAnalysis): Option[Attack] = {

    attacks.sortBy { attacks => (attacks.points, attacks.cards.map(_.rank.strength).min) }.headOption
  }

  private def findBestAttackContaining(attacks: List[Attack], preferredCards: List[Card]): Option[Attack] = {

    val preferredSet = preferredCards.toSet

    attacks
      .filter { attack => preferredSet.subsetOf(attack.cards.toSet)}
      .sortBy { attacks => (attacks.points, attacks.cards.map(_.rank.strength).min) }
      .headOption
      .orElse(chooseSmallestAttack(attacks,
        HandAnalysis(preferredCards, Nil, preferredCards, 0, 0, preferredCards.map(_.suit).distinct.size)))
  }
}
