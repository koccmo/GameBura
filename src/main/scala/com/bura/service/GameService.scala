package com.bura.service

import com.bura.game.{GameState, Round}
import com.bura.model.{Attack, Defense, Player}
import com.bura.rules.Rules

trait GameService {

  def attack(state: GameState, attack: Attack): Either[String, GameState]

  def defend(state: GameState, defense: Defense): Either[String, GameState]

  def surrender(state: GameState, defense: Defense): Either[String, GameState]
}

object GameService extends GameService {
  override def attack(state: GameState, attack: Attack): Either[String, GameState] =
    state.round match {
      case Round.WaitingAttack =>
        if (!attack.isValid) { Left("Attack cards must have the same suit") }
        else if (!attack.cards.forall(state.attackingPlayer.hand.contains)) { Left("Player does not have these cards") }
        else {
          val updatedState = state.modifyAttacker(_.playAttack(attack))

          Right(updatedState.withAttack(attack))
        }
      case _                   =>
        Left("It is not time to attack")
    }

  override def defend(state: GameState, defense: Defense): Either[String, GameState] =
    state.round match {
      case Round.WaitingDefense(attack) =>
        if (defense.cards.size != attack.cards.size) {
          Left("Defense must contain the same number of cards as the attack")
        } else if (!defense.cards.forall(state.defendingPlayer.hand.contains)) {
          Left("Player does not have these cards")
        } else {
          val updatedState: GameState = state.modifyDefender(_.playDefense(defense))
          val newState                =
            if (Rules.beatsAttack(attack, defense, state.trump)) { updatedState.defenderWon(attack, defense) }
            else updatedState.attackWon(attack, defense)

          finishRound(newState)
        }
      case _                            => Left("There is no attack to defend")
    }

  override def surrender(state: GameState, defense: Defense): Either[String, GameState] =
    state.round match {
      case Round.WaitingDefense(attack) =>
        if (defense.cards.size != attack.cards.size) { Left("You must place the same number of cards as the attack") }
        else if (!defense.cards.forall(state.defendingPlayer.hand.contains)) {
          Left("Player does not have these cards")
        } else {
          val updatedState: GameState = state.modifyDefender(_.playDefense(defense))
          val newState: GameState     = updatedState.attackerWonHidden(attack, defense)

          finishRound(newState)
        }
      case _                            => Left("There is no attack to surrender to")
    }

  private def finishRound(state: GameState): Either[String, GameState] =
    state.round match {
      case Round.DefenderWon(trick) =>
        Right(finishWithWinner(state, state.defender, _.addTrick(trick)))

      case Round.AttackerWon(trick) =>
        Right(finishWithWinner(state, state.attacker, _.addTrick(trick)))

      case Round.AttackerWonHidden(openTrick, hiddenTrick) =>
        Right(
          finishWithWinner(state, state.attacker, player =>
              player
                .addTrick(openTrick)
                .addHiddenTrick(hiddenTrick)
          )
        )
      case _                                               => Left("Round is not finished")
    }

  private def finishWithWinner(
    state: GameState,
    winnerIndex: Int,
    updatePlayer: Player => Player
  ): GameState =
    state
      .modifyPlayer(winnerIndex)(updatePlayer)
      .nextRound(winnerIndex)
      .refillHands
}
