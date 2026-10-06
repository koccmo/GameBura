package com.bura.game

import com.bura.ai.BotAi
import com.bura.ui.{ConsoleUI, DefenseAction}
import com.bura.model.{Attack, Player}
import com.bura.rules.BuraRules
import com.bura.service.GameService
import com.bura.ui.DeclareAction

import scala.annotation.tailrec

object GameEngine {

  def play(initialState: GameState): Either[String, FinishedGame] = {

    @annotation.tailrec
    def loop(state: GameState): Either[String, FinishedGame] =
      if (isGameOver(state)) {
        gameResult(state).map { result =>
          FinishedGame(state = state, result = result)
        }
      } else {
        playTurn(state) match {
          case Right(newState) => state.round match {

            case Round.WaitingDefense(_) => checkDeclaration(newState) match {
              case Left(error) => Left(error)
              case Right(Some(result)) => Right(FinishedGame(state = newState, result = result))
              case Right(None) => loop(newState)
                }

            case _ => loop(newState)
            }
          case Left(error) => Left(error)
        }
      }

    loop(initialState)
  }

  // TURN
  private def playTurn(state: GameState): Either[String, GameState] =
    state.round match {
      case Round.WaitingAttack          => playAttackTurn(state)
      case Round.WaitingDefense(attack) => playDefenseTurn(state, attack)
      case _                            => Left(s"Unexpected round state: ${state.round}")
    }

  // ATTACK TURN
  private def playAttackTurn(state: GameState): Either[String, GameState] =
    state.attackingPlayer match {
      case human: Player.Human => playHumanAttack(state, human)
      case robot: Player.Robot => playRobotAttack(state, robot)
    }

  // DEFENSE TURN
  private def playDefenseTurn(state: GameState, attack: Attack): Either[String, GameState] = {
    val result = state.defendingPlayer match {
      case human: Player.Human => playHumanDefense(state, human, attack)
      case robot: Player.Robot => playRobotDefense(state, robot, attack)
    }

    result.map{ newState =>
      ConsoleUI.showRoundResult(newState)
      newState
    }
  }

  // ROBOT ATTACK
  private def playRobotAttack(state: GameState, robot: Player.Robot): Either[String, GameState] =
    BotAi
      .chooseAttack(state, robot) match {
      case Some(attack) => GameService.attack(state, attack)
      case None         => Left("Robot could not choose an attack")
    }

  // ROBOT DEFENSE
  private def playRobotDefense(
    state: GameState,
    robot: Player.Robot,
    attack: Attack
  ): Either[String, GameState] =
    BotAi.chooseDefense(state, robot, attack) match {

      case Some(defense) => GameService.defend(state, defense)
      case None          => playRobotSurrender(state, robot, attack)
    }

  // ROBOT Surrender
  private def playRobotSurrender(
    state: GameState,
    robot: Player.Robot,
    attack: Attack
  ): Either[String, GameState] =
    BotAi.chooseSurrenderDefense(state, robot, attack) match {
      case Some(defense) => GameService.surrender(state, defense)
      case None          => Left("Robot could not choose surrender cards")
    }

  // HUMAN ATTACK
  private def playHumanAttack(state: GameState, human: Player.Human): Either[String, GameState] = {
    @tailrec
    def loop(): Either[String, GameState] =
      ConsoleUI.chooseAttack(state, human.hand) match {
        case Left(error)   =>
          println(error)
          loop()
        case Right(attack) =>
          GameService.attack(state, attack) match {
            case Right(newState) => Right(newState)
            case Left(error)     =>
              println(error)
              loop()

          }
      }
    loop()

  }

  // HUMAN DEFENSE
  private def playHumanDefense(
    state: GameState,
    human: Player.Human,
    attack: Attack
  ): Either[String, GameState] = {

    @tailrec
    def loop(): Either[String, GameState] = {
      ConsoleUI.chooseDefenseAction(state, attack, human.hand) match {
        case Left(error) =>
          println(error)
          loop()
        case Right(DefenseAction.Defend) => ConsoleUI.chooseDefense(
          hand = human.hand,
          amount = attack.cards.size
        ) match {
          case Left(error) =>
            println(error)
            loop()
          case Right(defense) => GameService.defend(state, defense) match {
            case Left(error) =>
              println(error)
              loop()
            case Right(newState) =>
              Right(newState)
          }
        }
        case Right(DefenseAction.Surrender) => ConsoleUI.chooseSurrender(
          hand = human.hand,
          amount = attack.cards.size
        ) match {
          case Left(error) =>
            println(error)
            loop()
          case Right(defense) => GameService.surrender(state, defense) match {
            case Left(error) =>
              println(error)
              loop()
            case Right(newState) =>
              Right(newState)
          }
        }
      }
    }
    loop()
  }

  // GAME OVER
  private def isGameOver(state: GameState): Boolean =
    state.deck.isEmpty && state.players.forall(_.hand.isEmpty)

  private def gameResult(state: GameState): Either[String, GameResult] =
    for {
      human <- state.players.collectFirst { case human: Player.Human => human }.toRight("Human player not found")
      robot <- state.players.collectFirst { case robot: Player.Robot => robot }.toRight("Robot player not found")
    } yield {

      val humanPoints = human.totalPoints
      val robotPoints = robot.totalPoints

      if (humanPoints > robotPoints) { GameResult.HumanWon(humanPoints, robotPoints) }
      else if (robotPoints > humanPoints) { GameResult.RobotWon(humanPoints, robotPoints) }
      else GameResult.Draw(humanPoints, robotPoints)
    }

  //DECLARATION
  private def checkDeclaration(
                                state: GameState
                              ): Either[String, Option[GameResult]] =
    state.attackingPlayer match {

      case human: Player.Human => humanDeclaration(state = state, human = human)
      case robot: Player.Robot => Right(robotDeclaration(state = state, robot = robot))
    }

  private def humanDeclaration(state: GameState, human: Player.Human): Either[String, Option[GameResult]] = {

    @annotation.tailrec
    def loop(): Either[String, Option[GameResult]] = { ConsoleUI.chooseDeclareAction(human) match {

        case Left(error) =>
          println(error)
          loop()

        case Right(DeclareAction.Continue) => Right(None)
        case Right(DeclareAction.Declare31) =>
          val humanPoints = human.totalPoints
          val robotPoints = state.players.collectFirst {
              case robot: Player.Robot => robot.totalPoints }.getOrElse(0)

          if (humanPoints >= BuraRules.WinningPoints) {

            Right(
              Some(
                GameResult.HumanWon(humanPoints = humanPoints, robotPoints = robotPoints)
              )
            )
          } else {
            Right(
              Some(
                GameResult.RobotWon(humanPoints = humanPoints, robotPoints = robotPoints)
              )
            )
          }
      }
    }

    loop()
  }

  private def robotDeclaration(state: GameState, robot: Player.Robot): Option[GameResult] = {

    if (!BotAi.shouldDeclare31(robot)) { None }
    else { println( s"${robot.name} declares ${BuraRules.WinningPoints}!")

      val robotPoints = robot.totalPoints

      val humanPoints = state.players.collectFirst {
          case human: Player.Human => human.totalPoints }.getOrElse(0)

      if (robotPoints >= BuraRules.WinningPoints) { Some(
        GameResult.RobotWon(humanPoints = humanPoints, robotPoints = robotPoints))}
      else { Some(
          GameResult.HumanWon(humanPoints = humanPoints, robotPoints = robotPoints)
        )
      }
    }
  }
}
