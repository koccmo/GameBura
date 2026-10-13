package com.bura.game

import com.bura.ai.BotAi
import com.bura.model.{Attack, Defense, Player}
import com.bura.service.{DeclarationService, GameService}


object GameEngine {


  //PLAYER ACTION
  def applyPlayerAction(state: GameState, action: PlayerAction): Either[String, ActionResult] =
    action match {
      case PlayerAction.AttackCards(attack)     => GameService.attack(state, attack).map(ActionResult.StateUpdated.apply)
      case PlayerAction.DefendCards(defense)    => GameService.defend(state, defense).map(ActionResult.StateUpdated.apply)
      case PlayerAction.SurrenderCards(defense) => GameService.surrender(state, defense).map(ActionResult.StateUpdated.apply)
      case PlayerAction.Continue                => Right(ActionResult.StateUpdated(state))
      case PlayerAction.Declare31               => state.attackingPlayer match {
          case human: Player.Human => state.players.collectFirst {
              case robot: Player.Robot => robot }
                .toRight("Robot player not found")
                .map { robot => val result =
                  DeclarationService.humanDeclarationResult(humanPoints = human.totalPoints, robotPoints = robot.totalPoints)

                  ActionResult.GameFinished(FinishedGame(state = state, result = result))
                }

          case robot: Player.Robot => state.players.collectFirst {
              case human: Player.Human => human }
                .toRight("Human player not found")
                .map { human => val result =
                  DeclarationService.robotDeclarationResult(humanPoints = human.totalPoints, robotPoints = robot.totalPoints)

                  ActionResult.GameFinished(FinishedGame(state = state, result = result))
               }
        }
    }

  // ROBOT ATTACK
  def playRobotAttack(state: GameState, robot: Player.Robot): Either[String, GameState] =
    BotAi.chooseAttack(state, robot) match {

      case Some(attack) => applyPlayerAction(state, PlayerAction.AttackCards(attack)) match {

          case Right(ActionResult.StateUpdated(newState)) => Right(newState)
          case Right(ActionResult.GameFinished(_)) => Left("Unexpected game finish during robot attack")
          case Left(error) => Left(error)
        }

      case None => Left("Robot could not choose an attack")
    }

  // ROBOT DEFENSE
  def playRobotDefense(
                        state: GameState,
                        robot: Player.Robot,
                        attack: Attack
                      ): Either[String, GameState] = BotAi.chooseDefense(state, robot, attack) match {

      case Some(defense) => applyPlayerAction(state, PlayerAction.DefendCards(defense)) match {

          case Right(ActionResult.StateUpdated(newState)) => Right(newState)
          case Right(ActionResult.GameFinished(_)) => Left("Unexpected game finish during robot defense")
          case Left(error) => Left(error)
        }

      case None => playRobotSurrender(state, robot, attack)
    }

  // ROBOT Surrender
  def playRobotSurrender(
                          state: GameState,
                          robot: Player.Robot,
                          attack: Attack
                        ): Either[String, GameState] =
    BotAi.chooseSurrenderDefense(state, robot, attack) match {

      case Some(defense) => applyPlayerAction(state, PlayerAction.SurrenderCards(defense)) match {

          case Right(ActionResult.StateUpdated(newState)) => Right(newState)
          case Right(ActionResult.GameFinished(_)) => Left("Unexpected game finish during robot surrender")
          case Left(error) => Left(error)
        }

      case None => Left("Robot could not choose surrender cards")
    }

  // GAME OVER
  def isGameOver(state: GameState): Boolean =
    state.deck.isEmpty && state.players.forall(_.hand.isEmpty)

  def gameResult(state: GameState): Either[String, GameResult] =
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

  def robotDeclaration(state: GameState, robot: Player.Robot): Option[GameResult] =
    if (!BotAi.shouldDeclare31(robot)) { None }
    else {

      val robotPoints = robot.totalPoints

      val humanPoints = state.players
        .collectFirst { case human: Player.Human =>
          human.totalPoints}
        .getOrElse(0)

      Some(DeclarationService.robotDeclarationResult(humanPoints = humanPoints, robotPoints = robotPoints))
    }
}
