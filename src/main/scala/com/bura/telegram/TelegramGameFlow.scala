package com.bura.telegram

import com.bot4s.telegram.models.Message
import com.bura.game.{GameEngine, GameResult, GameSession, GameState, PlayerAction, RobotDefenseResult, Round, SessionResult}
import com.bura.model.{Attack, Card, Defense, Player, Suit}

import scala.concurrent.{ExecutionContext, Future}
import TelegramKeyboards._
import TelegramMessages._

final class TelegramGameFlow(
                              botState: TelegramBotState,
                              telegramView: TelegramView
                            )(implicit ec: ExecutionContext) {

  def sendState(message: Message, session: GameSession): Future[Unit] = {

    val state = session.state
    val human = state.players.collectFirst { case human: Player.Human => human }
    val humanCards =
      human
        .map(_.hand.mkString(", "))
        .getOrElse("No cards")

    val keyboard = state.attackingPlayer match {
        case _: Player.Human =>
          human.map { player =>attackKeyboard(player = player, selected = Vector.empty) }

        case _: Player.Robot => None
      }

    val humanPoints = human
        .map(_.totalPoints)
        .getOrElse(0)

    telegramView.sendMessage(
      message = message,
      text = gameState(
        trump = suitSymbol(state.trump),
        attacker = state.attackingPlayer.name,
        defender = state.defendingPlayer.name,
        humanPoints = humanPoints,
        humanCards = humanCards
      ),
      keyboard = keyboard
    )
  }

  def playRobotAttackTurn(message: Message, state: GameState, chatId: Long): Future[Unit] =
    state.attackingPlayer match {
      case robot: Player.Robot =>

        GameEngine.playRobotAttack(state, robot) match {

          case Right(stateAfterRobot) =>

            botState.store = botState.store.put(chatId, GameSession(stateAfterRobot))

            stateAfterRobot.round match {
              case Round.WaitingDefense(attack) =>

                val human = stateAfterRobot.players.collectFirst { case human: Player.Human => human }

                val keyboard = human.map { player =>defenseKeyboard(player = player, selected = Vector.empty) }

                telegramView.sendMessage(
                  message = message,
                  text = robotAttack(attack.cards.mkString(", ")),
                  keyboard = keyboard
                )

              case _ =>
                telegramView.sendMessage(
                  message = message,
                  text = unexpectedRobotAttackState,
                  keyboard = None
                )
            }

          case Left(error) =>
            telegramView.sendMessage(
              message = message,
              text = robotAttackError(error),
              keyboard = None
            )
        }

      case _: Player.Human => sendState(message = message, session = GameSession(state))
    }

  def continueGame(message: Message, state: GameState, chatId: Long): Future[Unit] = {

    if (GameEngine.isGameOver(state)) { GameEngine.gameResult(state) match {
      case Right(result) =>

          botState.store = botState.store.remove(chatId)

          val resultText = result match {
            case GameResult.HumanWon(humanPoints, robotPoints) =>
                humanWon(humanPoints, robotPoints)

              case GameResult.RobotWon(humanPoints, robotPoints) =>
                robotWon(humanPoints, robotPoints)

              case GameResult.Draw(humanPoints, robotPoints) =>
                draw(humanPoints, robotPoints)
            }

          telegramView.sendMessage(message = message, text = resultText, keyboard = Some(playAgainKeyboard))

        case Left(error) =>
          telegramView.sendMessage(
            message = message,
            text = gameResultError(error),
            keyboard = None
          )
      }

    } else {
      state.attackingPlayer match {

        case robot: Player.Robot =>

          GameEngine.robotDeclaration(state = state, robot = robot) match {
            case Some(result) =>

              botState.store = botState.store.remove(chatId)

              val resultText = result match {
                  case GameResult.RobotWon(humanPoints, robotPoints) =>
                    robotDeclared31Won(humanPoints, robotPoints)

                  case GameResult.HumanWon(humanPoints, robotPoints) =>
                    robotDeclared31Lost(humanPoints, robotPoints)

                  case GameResult.Draw(humanPoints, robotPoints) =>
                    draw(humanPoints, robotPoints)
                }

              telegramView.sendMessage(message = message, text = resultText, keyboard = Some(playAgainKeyboard))

            case None =>
              playRobotAttackTurn(message = message, state = state, chatId = chatId)
          }

        case _: Player.Human =>
          sendState(message = message, session = GameSession(state))
      }
    }
  }

  def handleDefenseAction(
                           message: Message,
                           chatId: Long,
                           answerCallback: Future[Unit],
                           emptySelectionMessage: String,
                           invalidRoundMessage: String,
                           actionBuilder: Defense => PlayerAction,
                           successMessage: List[Card] => String,
                           actionErrorMessage: String => String
                         ): Future[Unit] = {

    val selectedIndexes =
      botState.selectedDefenseCards.getOrElse(chatId, Vector.empty)

    if (selectedIndexes.isEmpty) {

      answerCallback.flatMap { _ =>
        telegramView.sendMessage(message = message, text = emptySelectionMessage, keyboard = None) }

    } else {
      botState.store.get(chatId) match {
        case Some(session) =>
          val state = session.state

          val human = state.players.collectFirst { case human: Player.Human => human }

          state.round match {

            case Round.WaitingDefense(_) => human match {
                case Some(player) =>

                  val cards = selectedIndexes.flatMap(player.hand.lift).toList

                  val defense = Defense(cards)

                  botState.store.applyAction(id = chatId, action = actionBuilder(defense)) match {
                    case Right((updatedStore, SessionResult.Updated(updatedSession))) =>
                      botState.store = updatedStore
                      botState.selectedDefenseCards = botState.selectedDefenseCards - chatId

                      answerCallback
                        .flatMap { _ => telegramView.clearKeyboard(message)}
                        .flatMap { _ => telegramView.sendMessage(
                            message = message,
                            text = successMessage(cards),
                            keyboard = None
                          )
                        }
                        .flatMap { _ => continueGame(
                            message = message,
                            state = updatedSession.state,
                            chatId = chatId
                          )
                        }

                    case Right((updatedStore, SessionResult.Finished(game))) =>
                      botState.store = updatedStore
                      botState.selectedDefenseCards = botState.selectedDefenseCards - chatId

                      answerCallback.flatMap { _ =>
                        telegramView.sendMessage(
                          message = message,
                          text = gameFinished(game.result.toString),
                          keyboard = None
                        )
                      }

                    case Left(error) =>
                      answerCallback.flatMap { _ =>
                        telegramView.sendMessage(
                          message = message,
                          text = actionErrorMessage(error),
                          keyboard = None
                        )
                      }
                  }

                case None => answerCallback
              }

            case _ =>

              answerCallback.flatMap { _ =>
                telegramView.sendMessage(
                  message = message,
                  text = invalidRoundMessage,
                  keyboard = None
                )
              }
          }

        case None =>

          answerCallback.flatMap { _ =>
            telegramView.sendMessage(
              message = message,
              text = gameNotFound,
              keyboard = None
            )
          }
      }
    }
  }

  def handleAttackAction(message: Message, chatId: Long, answerCallback: Future[Unit]): Future[Unit] = {

    val selectedIndexes =
      botState.selectedAttackCards.getOrElse(chatId, Vector.empty)

    if (selectedIndexes.isEmpty) {
      answerCallback.flatMap { _ =>
        telegramView.sendMessage(
          message = message,
          text = selectAttackCards,
          keyboard = None
        )
      }

    } else {

      botState.store.get(chatId) match {
        case Some(session) =>

          val human = session.state.players.collectFirst {
              case human: Player.Human => human
            }

          human match {
            case Some(player) =>
              val cards = selectedIndexes.flatMap(player.hand.lift).toList
              val attack = Attack(cards)

              botState.store.applyAction(id = chatId, action = PlayerAction.AttackCards(attack)) match {

                case Right((updatedStore, SessionResult.Updated(updatedSession))) =>
                  botState.store = updatedStore

                  botState.selectedAttackCards = botState.selectedAttackCards - chatId

                  val robot =
                    updatedSession.state.players.collectFirst {
                      case robot: Player.Robot => robot
                    }

                  robot match {
                    case Some(robotPlayer) =>

                      GameEngine.playRobotDefenseResult(
                        state = updatedSession.state,
                        robot = robotPlayer,
                        attack = attack
                      ) match {

                        case Right(RobotDefenseResult.Defended(stateAfterRobot, defense)) =>
                          botState.store =
                            botState.store.put(chatId, GameSession(stateAfterRobot))

                          answerCallback
                            .flatMap { _ =>telegramView.clearKeyboard(message)}
                            .flatMap { _ =>telegramView.sendMessage(
                                message = message,
                                text = humanAttackRobotDefended(
                                  attackCards = cards.mkString(", "),
                                  defenseCards = defense.cards.mkString(", ")),
                                keyboard = None
                              )
                            }
                            .flatMap { _ =>
                              continueGame(
                                message = message,
                                state = stateAfterRobot,
                                chatId = chatId
                              )
                            }

                        case Right(RobotDefenseResult.Surrendered(stateAfterRobot)) =>

                          botState.store =
                            botState.store.put(chatId, GameSession(stateAfterRobot))

                          answerCallback
                            .flatMap { _ =>telegramView.clearKeyboard(message)}
                            .flatMap { _ =>
                              telegramView.sendMessage(
                                message = message,
                                text = humanAttackRobotSurrendered(cards.mkString(", ")),
                                keyboard = None
                              )
                            }
                            .flatMap { _ =>
                              continueGame(
                                message = message,
                                state = stateAfterRobot,
                                chatId = chatId
                              )
                            }

                        case Left(error) =>

                          answerCallback.flatMap { _ =>
                            telegramView.sendMessage(
                              message = message,
                              text = robotTurnError(error),
                              keyboard = None
                            )
                          }
                      }

                    case None =>

                      answerCallback.flatMap { _ =>
                        telegramView.sendMessage(
                          message = message,
                          text = robotNotFound,
                          keyboard = None
                        )
                      }
                  }

                case Right((updatedStore, SessionResult.Finished(game))) =>
                  botState.store = updatedStore

                  botState.selectedAttackCards = botState.selectedAttackCards - chatId

                  answerCallback
                    .flatMap { _ =>telegramView.clearKeyboard(message)}
                    .flatMap { _ =>
                      telegramView.sendMessage(
                        message = message,
                        text = gameFinished(game.result.toString),
                        keyboard = None
                      )
                    }

                case Left(error) =>

                  answerCallback.flatMap { _ =>
                    telegramView.sendMessage(
                      message = message,
                      text = attackError(error),
                      keyboard = None
                    )
                  }
              }

            case None => answerCallback
          }

        case None =>

          answerCallback.flatMap { _ =>
            telegramView.sendMessage(
              message = message,
              text = gameNotFound,
              keyboard = None
            )
          }
      }
    }
  }

  def handleDeclare31(message: Message, chatId: Long, answerCallback: Future[Unit]): Future[Unit] = {

    botState.store.applyAction(id = chatId, action = PlayerAction.Declare31) match {
      case Right((updatedStore, SessionResult.Finished(game))) =>
        botState.store = updatedStore

        botState.selectedAttackCards = botState.selectedAttackCards - chatId

        val resultText = game.result match {

            case GameResult.HumanWon(humanPoints, robotPoints) =>
              humanDeclared31Won(humanPoints, robotPoints)

            case GameResult.RobotWon(humanPoints, robotPoints) =>
              humanDeclared31Lost(humanPoints, robotPoints)

            case GameResult.Draw(humanPoints, robotPoints) =>
              draw(humanPoints, robotPoints)
          }

        answerCallback
          .flatMap { _ =>telegramView.clearKeyboard(message)}
          .flatMap { _ =>
            telegramView.sendMessage(
              message = message,
              text = resultText,
              keyboard = Some(playAgainKeyboard)
            )
          }

      case Right((updatedStore, SessionResult.Updated(updatedSession))) =>
        botState.store = updatedStore

        answerCallback.flatMap { _ =>
          sendState(
            message = message,
            session = updatedSession
          )
        }

      case Left(error) =>

        answerCallback.flatMap { _ =>
          telegramView.sendMessage(
            message = message,
            text = declarationError(error),
            keyboard = None
          )
        }
    }
  }

  private def suitSymbol(suit: Suit): String =
    suit match {
      case Suit.Clubs => "♣️"
      case Suit.Diamonds => "♦️"
      case Suit.Hearts => "♥️"
      case Suit.Spades => "♠️"
    }
}