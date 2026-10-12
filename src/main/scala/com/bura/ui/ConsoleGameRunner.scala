package com.bura.ui


import com.bura.game.GameEngine.applyPlayerAction
import com.bura.game.{ActionResult, FinishedGame, GameEngine, GameResult, GameState, PlayerAction, Round}
import com.bura.model.{Attack, Player}
import com.bura.rules.BuraRules

import scala.annotation.tailrec

object ConsoleGameRunner {

  def play(initialState: GameState): Either[String, FinishedGame] = {

    @tailrec
    def loop(state: GameState): Either[String, FinishedGame] = {
      if (GameEngine.isGameOver(state)) {
        GameEngine
          .gameResult(state)
          .map(result => FinishedGame(state = state, result = result))
      } else {
        playTurn(state) match {

          case Left(error) => Left(error)
          case Right(newState) => state.round match {
            case Round.WaitingDefense(_) => checkDeclaration(newState) match {
              case Left(error) => Left(error)
              case Right(Some(result)) => Right(FinishedGame(state = newState, result = result))

              case Right(None) =>
                loop(newState)
            }

            case _ =>
              loop(newState)
          }
        }
      }
    }

    loop(initialState)
  }

//TURN
  private def playTurn(state: GameState): Either[String, GameState] = state.round match {

    case Round.WaitingAttack => playAttackTurn(state)
    case Round.WaitingDefense(attack) => playDefenseTurn(state, attack)
    case _ => Left(s"Unexpected round state: ${state.round}")
  }

//ATTACK TURN
  private def playAttackTurn(state: GameState): Either[String, GameState] = state.attackingPlayer match {

    case human: Player.Human => playHumanAttack(state, human)
    case robot: Player.Robot => GameEngine.playRobotAttack(state, robot)}

//DEFENSE TURN
  private def playDefenseTurn(state: GameState, attack: Attack): Either[String, GameState] = {

    val result = state.defendingPlayer match {

      case human: Player.Human => playHumanDefense(state, human, attack)
      case robot: Player.Robot => GameEngine.playRobotDefense(state, robot, attack)
    }

    result.map { newState => showRoundResult(newState)
      newState
    }
  }

  //HUMAN PLAY
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

//DECLARATION
  private def checkDeclaration(state: GameState): Either[String, Option[GameResult]] = state.attackingPlayer match {

    case human: Player.Human => humanDeclaration(state = state, human = human)
    case robot: Player.Robot => Right(GameEngine.robotDeclaration(state, robot))
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

  def showRobotDeclaration(robot: Player.Robot): Unit =
    println(s"${robot.name} declares ${BuraRules.WinningPoints}!")



}
