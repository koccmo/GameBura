package com.bura.ui


import com.bura.game.GameEngine.applyPlayerAction
import com.bura.game.{ActionResult, GameEngine, GameResult, GameState, PlayerAction}
import com.bura.model.{Attack, Player}

import scala.annotation.tailrec

object ConsoleGameRunner {

  def playHumanAttack(state: GameState, human: Player.Human): Either[String, GameState] = {
    @tailrec
    def loop(): Either[String, GameState] =
      ConsoleUI.chooseAttack(state, human.hand) match {
        case Left(error)   =>
          println(error)
          loop()
        case Right(attack) =>
          GameEngine.applyPlayerAction(state, PlayerAction.AttackCards(attack)) match {
            case Right(ActionResult.StateUpdated(newState)) => Right(newState)
            case Right(ActionResult.GameFinished(_))        => Left("Unexpected game finish during attack")
            case Left(error)                                =>
              println(error)
              loop()

          }
      }
    loop()

  }

  def playHumanDefense(
                                state: GameState,
                                human: Player.Human,
                                attack: Attack
                              ): Either[String, GameState] = {

    @tailrec
    def loop(): Either[String, GameState] =
      ConsoleUI.chooseDefenseAction(state, attack, human.hand) match {
        case Left(error)                    =>
          println(error)
          loop()
        case Right(DefenseAction.Defend)    =>
          ConsoleUI.chooseDefense(hand = human.hand, amount = attack.cards.size) match {
            case Left(error)    =>
              println(error)
              loop()
            case Right(defense) =>
              GameEngine.applyPlayerAction(state, PlayerAction.DefendCards(defense)) match {
                case Left(error)                                =>
                  println(error)
                  loop()
                case Right(ActionResult.StateUpdated(newState)) => Right(newState)
              }
          }
        case Right(DefenseAction.Surrender) =>
          ConsoleUI.chooseSurrender(hand = human.hand, amount = attack.cards.size) match {
            case Left(error)    =>
              println(error)
              loop()
            case Right(defense) =>
              GameEngine.applyPlayerAction(state, PlayerAction.SurrenderCards(defense)) match {
                case Left(error)                                =>
                  println(error)
                  loop()
                case Right(ActionResult.StateUpdated(newState)) => Right(newState)
                case Right(ActionResult.GameFinished(_))        => Left("Unexpected game finish during surrender")
              }
          }
      }
    loop()
  }

  def humanDeclaration(state: GameState, human: Player.Human): Either[String, Option[GameResult]] = {

    @tailrec
    def loop(): Either[String, Option[GameResult]] = ConsoleUI.chooseDeclareAction(human) match {

        case Left(error) =>
          println(error)
          loop()

        case Right(DeclareAction.Continue) => GameEngine.applyPlayerAction(state, PlayerAction.Continue) match {
            case Right(ActionResult.StateUpdated(_)) => Right(None)
            case Right(ActionResult.GameFinished(game)) => Right(Some(game.result))
            case Left(error) => Left(error)
          }

        case Right(DeclareAction.Declare31) => GameEngine.applyPlayerAction(state, PlayerAction.Declare31) match {
            case Right(ActionResult.GameFinished(game)) => Right(Some(game.result))
            case Right(ActionResult.StateUpdated(_)) => Left("Declare31 should finish the game")
            case Left(error) => Left(error)
          }
      }

    loop()
  }

  def showRoundResult(state: GameState): Unit =
    ConsoleUI.showRoundResult(state)

}
