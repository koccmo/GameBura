package com.bura.telegram

import cats.syntax.functor._
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.clients.FutureSttpClient
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup, Message}
import com.bura.game.{GameEngine, GameResult, GameSession, GameSessionStore, GameState, PlayerAction, RobotDefenseResult, Round, SessionResult}
import com.bura.model.{Attack, Defense, Player, Suit}
import sttp.client4.Backend
import sttp.client4.okhttp.OkHttpFutureBackend
import com.bot4s.telegram.methods.{AnswerCallbackQuery, SendMessage}
import com.bot4s.telegram.models.{CallbackQuery, InlineKeyboardButton, InlineKeyboardMarkup, Message}
import com.bot4s.telegram.methods.{AnswerCallbackQuery, EditMessageReplyMarkup, SendMessage}
import io.circe.Decoder
import com.bot4s.telegram.Implicits._

import scala.concurrent.Future

class BuraTelegramBot(token: String) extends TelegramBot with Polling {

  private var store                                                          = GameSessionStore()
  implicit val editMessageResponseDecoder: Decoder[Either[Boolean, Message]] =
    Decoder[Boolean].either(Decoder[Message])

  private var selectedAttackCards: Map[Long, Vector[Int]] = Map.empty
  private var selectedDefenseCards: Map[Long, Vector[Int]] = Map.empty

  implicit val backend: Backend[Future] = OkHttpFutureBackend()

  override val client: RequestHandler[Future] = new FutureSttpClient(token)

  private def sendState(
                         message: Message,
                         session: GameSession
                       ): Future[Unit] = {

    val state = session.state

    val human =
      state.players.collectFirst {
        case human: Player.Human =>
          human
      }

    val humanCards =
      human
        .map(_.hand.mkString(", "))
        .getOrElse("No cards")

    val keyboard =
      state.attackingPlayer match {

        case _: Player.Human =>
          human.map { player =>
            attackKeyboard(
              player = player,
              selected = Vector.empty
            )
          }

        case _: Player.Robot =>
          None
      }

    val humanPoints = human.map(_.totalPoints).getOrElse(0)

    request(
      SendMessage(
        message.source,
        s"""Козырь: ${suitSymbol(state.trump)}
           |Атакует: ${state.attackingPlayer.name}
           |Защищается: ${state.defendingPlayer.name}
           |Твои очки: $humanPoints
           |Твои карты: $humanCards""".stripMargin,
        replyMarkup = keyboard
      )
    ).void
  }

  private def playRobotAttackTurn(
                                   message: Message,
                                   state: GameState,
                                   chatId: Long
                                 ): Future[Unit] = {

    state.attackingPlayer match {

      case robot: Player.Robot =>

        GameEngine.playRobotAttack(
          state,
          robot
        ) match {

          case Right(stateAfterRobot) =>

            store =
              store.put(
                chatId,
                GameSession(stateAfterRobot)
              )

            stateAfterRobot.round match {

              case Round.WaitingDefense(attack) =>

                val human =
                  stateAfterRobot.players.collectFirst {
                    case human: Player.Human =>
                      human
                  }

                val keyboard =
                  human.map { player =>
                    defenseKeyboard(
                      player = player,
                      selected = Vector.empty
                    )
                  }

                request(
                  SendMessage(
                    message.source,
                    s"Робот атакует: ${attack.cards.mkString(", ")}",
                    replyMarkup = keyboard
                  )
                ).void

              case _ =>

                request(
                  SendMessage(
                    message.source,
                    "Ошибка: после атаки робота ожидалась защита."
                  )
                ).void
            }

          case Left(error) =>

            request(
              SendMessage(
                message.source,
                s"Ошибка атаки робота: $error"
              )
            ).void
        }

      case _: Player.Human =>
        sendState(
          message = message,
          session = GameSession(state)
        )
    }
  }

  private def continueGame(
                            message: Message,
                            state: GameState,
                            chatId: Long
                          ): Future[Unit] = {

    if (GameEngine.isGameOver(state)) {

      GameEngine.gameResult(state) match {

        case Right(result) =>

          store =
            store.remove(chatId)

          val resultText =
            result match {

              case GameResult.HumanWon(
                humanPoints,
                robotPoints
              ) =>
                s"""🏆 Ты выиграл!
                   |Твои очки: $humanPoints
                   |Очки робота: $robotPoints""".stripMargin

              case GameResult.RobotWon(
                humanPoints,
                robotPoints
              ) =>
                s"""🤖 Робот выиграл.
                   |Твои очки: $humanPoints
                   |Очки робота: $robotPoints""".stripMargin

              case GameResult.Draw(
                humanPoints,
                robotPoints
              ) =>
                s"""🤝 Ничья.
                   |Твои очки: $humanPoints
                   |Очки робота: $robotPoints""".stripMargin
            }

          request(
            SendMessage(
              message.source,
              resultText
            )
          ).void

        case Left(error) =>

          request(
            SendMessage(
              message.source,
              s"Ошибка определения результата: $error"
            )
          ).void
      }

    } else {

      state.attackingPlayer match {

        case robot: Player.Robot =>

          GameEngine.robotDeclaration(
            state = state,
            robot = robot
          ) match {

            case Some(result) =>

              store =
                store.remove(chatId)

              val resultText =
                result match {

                  case GameResult.RobotWon(
                    humanPoints,
                    robotPoints
                  ) =>
                    s"""🤖 Робот объявил 31 и выиграл!
                       |Твои очки: $humanPoints
                       |Очки робота: $robotPoints""".stripMargin

                  case GameResult.HumanWon(
                    humanPoints,
                    robotPoints
                  ) =>
                    s"""🏆 Робот ошибочно объявил 31.
                       |Ты выиграл!
                       |Твои очки: $humanPoints
                       |Очки робота: $robotPoints""".stripMargin

                  case GameResult.Draw(
                    humanPoints,
                    robotPoints
                  ) =>
                    s"""🤝 Ничья.
                       |Твои очки: $humanPoints
                       |Очки робота: $robotPoints""".stripMargin
                }

              request(
                SendMessage(
                  message.source,
                  resultText
                )
              ).void


            case None =>

              playRobotAttackTurn(
                message = message,
                state = state,
                chatId = chatId
              )
          }

        case _: Player.Human =>
          sendState(
            message = message,
            session = GameSession(state)
          )
      }
    }
  }

  override def receiveMessage(msg: Message): Future[Unit] =
    msg.text match {

      case Some("/start") =>
        request(
          SendMessage(
            msg.source,
            """Привет! Я бот для игры в Буру.
              |
              |Команды:
              |/newgame — начать новую игру
              |/state — показать текущее состояние
              |/help — помощь
              |
              |Во время игры используй кнопки под сообщениями.""".stripMargin
          )
        ).void

      case Some("/help") =>
        request(
          SendMessage(
            msg.source,
            """Как играть:
              |
              |• Выбери от 1 до 3 карт одной масти и нажми «Атаковать».
              |• Если атакует робот — выбери карты и нажми «Отбиться».
              |• Если не хочешь отбиваться — выбери нужное количество карт и нажми «Сдаться».
              |• Если у тебя 31 или больше очков — можно нажать «Объявить 31».
              |
              |Команды:
              |/newgame — новая игра
              |/state — текущее состояние
              |/help — эта справка""".stripMargin
          )
        ).void
        
      case Some("/newgame") =>
        val chatId = msg.chat.id

        selectedAttackCards = selectedAttackCards - chatId

        selectedDefenseCards = selectedDefenseCards - chatId

        store.createGame(id = chatId, humanName = "Human") match {

          case Right(updatedStore) =>
            store = updatedStore
            request(SendMessage(msg.source, "Новая игра создана!")).void

          case Left(error) => request(SendMessage(msg.source, s"Ошибка: $error")).void
        }

      case Some("/state") => val chatId =
        msg.chat.id

        store.get(chatId) match {
          case Some(session) => sendState(message = msg, session = session)
          case None => request(SendMessage(msg.source, "Игра не найдена. Сначала используй /newgame")).void
        }

      case _ => Future.successful(())
    }

  override def receiveCallbackQuery(
    callbackQuery: CallbackQuery
  ): Future[Unit] = {

    val answerCallback =
      request(
        AnswerCallbackQuery(callbackQuery.id)
      ).void

    (callbackQuery.message, callbackQuery.data) match {

      // ВЫБОР КАРТЫ
      case (Some(message), Some(data)) if data.startsWith("select-attack:") =>
        val chatId =
          message.chat.id

        val indexOption =
          data
            .stripPrefix("select-attack:")
            .toIntOption

        indexOption match {

          case Some(index) =>
            val currentSelection =
              selectedAttackCards.getOrElse(
                chatId,
                Vector.empty
              )

            val newSelection =
              if (currentSelection.contains(index)) {

                // Повторное нажатие снимает выбор
                currentSelection.filterNot(_ == index)

              } else if (currentSelection.size < 3) {

                // Добавляем карту
                currentSelection :+ index

              } else {

                // Уже выбрано 3 карты
                currentSelection
              }

            selectedAttackCards = selectedAttackCards.updated(
              chatId,
              newSelection
            )

            answerCallback.flatMap { _ =>
              store.get(chatId) match {

                case Some(session) =>
                  val human =
                    session.state.players.collectFirst { case human: Player.Human => human }

                  human match {

                    case Some(player) =>
                      val keyboard =
                        attackKeyboard(
                          player = player,
                          selected = newSelection
                        )

                      request(
                        EditMessageReplyMarkup(
                          chatId = Some(message.source),
                          messageId = Some(message.messageId),
                          replyMarkup = Some(keyboard)
                        )
                      ).void

                    case None =>
                      Future.successful(())
                  }

                case None =>
                  Future.successful(())
              }
            }

          case None =>
            answerCallback
        }
      //Выбор карт для атаки
      case (Some(message), Some(data))
        if data.startsWith("select-defense:") =>

        val chatId =
          message.chat.id

        val indexOption =
          data
            .stripPrefix("select-defense:")
            .toIntOption

        indexOption match {

          case Some(index) =>

            val currentSelection =
              selectedDefenseCards.getOrElse(
                chatId,
                Vector.empty
              )

            val newSelection =
              if (currentSelection.contains(index)) {

                currentSelection.filterNot(_ == index)

              } else if (currentSelection.size < 3) {

                currentSelection :+ index

              } else {

                currentSelection
              }

            selectedDefenseCards =
              selectedDefenseCards.updated(
                chatId,
                newSelection
              )

            answerCallback.flatMap { _ =>

              store.get(chatId) match {

                case Some(session) =>

                  val human =
                    session.state.players.collectFirst {
                      case human: Player.Human =>
                        human
                    }

                  human match {

                    case Some(player) =>

                      val keyboard =
                        defenseKeyboard(
                          player = player,
                          selected = newSelection
                        )

                      request(
                        EditMessageReplyMarkup(
                          chatId = Some(message.source),
                          messageId = Some(message.messageId),
                          replyMarkup = Some(keyboard)
                        )
                      ).void

                    case None =>
                      Future.successful(())
                  }

                case None =>
                  Future.successful(())
              }
            }

          case None =>
            answerCallback
        }

      //Кнопка «Отбиться»
      case (Some(message), Some("defense:confirm")) =>

        val chatId =
          message.chat.id

        val selectedIndexes =
          selectedDefenseCards.getOrElse(
            chatId,
            Vector.empty
          )

        if (selectedIndexes.isEmpty) {

          answerCallback.flatMap { _ =>
            request(
              SendMessage(
                message.source,
                "Сначала выбери карты для защиты."
              )
            ).void
          }

        } else {

          store.get(chatId) match {

            case Some(session) =>

              val state =
                session.state

              val human =
                state.players.collectFirst {
                  case human: Player.Human =>
                    human
                }

              state.round match {

                case Round.WaitingDefense(attack) =>

                  human match {

                    case Some(player) =>

                      val cards =
                        selectedIndexes
                          .flatMap(player.hand.lift)
                          .toList

                      val defense =
                        Defense(cards)

                      store.applyAction(
                        id = chatId,
                        action = PlayerAction.DefendCards(defense)
                      ) match {

                        case Right(
                          (
                            updatedStore,
                            SessionResult.Updated(updatedSession)
                          )
                        ) =>

                          store = updatedStore

                          selectedDefenseCards =
                            selectedDefenseCards - chatId

                          answerCallback
                            .flatMap { _ =>
                              clearKeyboard(message)
                            }
                            .flatMap { _ =>
                              request(
                                SendMessage(
                                  message.source,
                                  s"Ты отбился: ${cards.mkString(", ")}"
                                )
                              ).void
                            }
                            .flatMap { _ =>
                              continueGame(
                                message = message,
                                state = updatedSession.state,
                                chatId = chatId
                              )
                            }

                        case Right(
                          (
                            updatedStore,
                            SessionResult.Finished(game)
                          )
                        ) =>

                          store = updatedStore

                          selectedDefenseCards =
                            selectedDefenseCards - chatId

                          answerCallback.flatMap { _ =>
                            request(
                              SendMessage(
                                message.source,
                                s"Игра закончена: ${game.result}"
                              )
                            ).void
                          }

                        case Left(error) =>

                          answerCallback.flatMap { _ =>
                            request(
                              SendMessage(
                                message.source,
                                s"Ошибка защиты: $error"
                              )
                            ).void
                          }
                      }

                    case None =>
                      answerCallback
                  }

                case _ =>

                  answerCallback.flatMap { _ =>
                    request(
                      SendMessage(
                        message.source,
                        "Сейчас не время защищаться."
                      )
                    ).void
                  }
              }

            case None =>

              answerCallback.flatMap { _ =>
                request(
                  SendMessage(
                    message.source,
                    "Игра не найдена. Используй /newgame"
                  )
                ).void
              }
          }
        }
      //Кнопка «Сдаться»
      case (Some(message), Some("defense:surrender")) =>

        val chatId =
          message.chat.id

        val selectedIndexes =
          selectedDefenseCards.getOrElse(
            chatId,
            Vector.empty
          )

        if (selectedIndexes.isEmpty) {

          answerCallback.flatMap { _ =>
            request(
              SendMessage(
                message.source,
                "Сначала выбери карты, которые хочешь сдать."
              )
            ).void
          }

        } else {

          store.get(chatId) match {

            case Some(session) =>

              val state =
                session.state

              val human =
                state.players.collectFirst {
                  case human: Player.Human =>
                    human
                }

              state.round match {

                case Round.WaitingDefense(_) =>

                  human match {

                    case Some(player) =>

                      val cards =
                        selectedIndexes
                          .flatMap(player.hand.lift)
                          .toList

                      val defense =
                        Defense(cards)

                      store.applyAction(
                        id = chatId,
                        action =
                          PlayerAction.SurrenderCards(defense)
                      ) match {

                        case Right(
                          (
                            updatedStore,
                            SessionResult.Updated(updatedSession)
                          )
                        ) =>

                          store = updatedStore

                          selectedDefenseCards =
                            selectedDefenseCards - chatId

                          val surrenderedText =
                            cards.size match {
                              case 1 => "1 карту"
                              case 2 => "2 карты"
                              case 3 => "3 карты"
                            }

                          answerCallback
                            .flatMap { _ =>
                              clearKeyboard(message)
                            }
                            .flatMap { _ =>
                              request(
                                SendMessage(
                                  message.source,
                                  s"Ты сдал $surrenderedText."
                                )
                              ).void
                            }
                            .flatMap { _ =>
                              continueGame(
                                message = message,
                                state = updatedSession.state,
                                chatId = chatId
                              )
                            }

                        case Right(
                          (
                            updatedStore,
                            SessionResult.Finished(game)
                          )
                        ) =>

                          store = updatedStore

                          selectedDefenseCards =
                            selectedDefenseCards - chatId

                          answerCallback.flatMap { _ =>
                            request(
                              SendMessage(
                                message.source,
                                s"Игра закончена: ${game.result}"
                              )
                            ).void
                          }

                        case Left(error) =>

                          answerCallback.flatMap { _ =>
                            request(
                              SendMessage(
                                message.source,
                                s"Ошибка сдачи: $error"
                              )
                            ).void
                          }
                      }

                    case None =>
                      answerCallback
                  }

                case _ =>

                  answerCallback.flatMap { _ =>
                    request(
                      SendMessage(
                        message.source,
                        "Сейчас нельзя сдаться."
                      )
                    ).void
                  }
              }

            case None =>

              answerCallback.flatMap { _ =>
                request(
                  SendMessage(
                    message.source,
                    "Игра не найдена. Используй /newgame"
                  )
                ).void
              }
          }
        }
      //Declare 31
      case (Some(message), Some("declare:31")) =>

        val chatId =
          message.chat.id

        store.applyAction(
          id = chatId,
          action = PlayerAction.Declare31
        ) match {

          case Right(
            (
              updatedStore,
              SessionResult.Finished(game)
            )
          ) =>

            store = updatedStore

            selectedAttackCards =
              selectedAttackCards - chatId

            val resultText =
              game.result match {

                case GameResult.HumanWon(
                  humanPoints,
                  robotPoints
                ) =>
                  s"""🏆 Ты объявил 31 и выиграл!
                     |Твои очки: $humanPoints
                     |Очки робота: $robotPoints""".stripMargin

                case GameResult.RobotWon(
                  humanPoints,
                  robotPoints
                ) =>
                  s"""❌ Ты объявил 31 слишком рано.
                     |Робот выиграл.
                     |Твои очки: $humanPoints
                     |Очки робота: $robotPoints""".stripMargin

                case GameResult.Draw(
                  humanPoints,
                  robotPoints
                ) =>
                  s"""🤝 Ничья.
                     |Твои очки: $humanPoints
                     |Очки робота: $robotPoints""".stripMargin
              }

            answerCallback
              .flatMap { _ =>
                clearKeyboard(message)
              }
              .flatMap { _ =>
                request(
                  SendMessage(
                    message.source,
                    resultText
                  )
                ).void
              }


          case Right(
            (
              updatedStore,
              SessionResult.Updated(updatedSession)
            )
          ) =>

            store = updatedStore

            answerCallback.flatMap { _ =>
              sendState(
                message = message,
                session = updatedSession
              )
            }


          case Left(error) =>

            answerCallback.flatMap { _ =>
              request(
                SendMessage(
                  message.source,
                  s"Ошибка объявления 31: $error"
                )
              ).void
            }
        }
      // ПОДТВЕРЖДЕНИЕ АТАКИ
      case (Some(message), Some("attack:confirm"))                          =>
        val chatId =
          message.chat.id

        val selectedIndexes =
          selectedAttackCards.getOrElse(
            chatId,
            Vector.empty
          )

        if (selectedIndexes.isEmpty) {

          answerCallback.flatMap { _ =>
            request(
              SendMessage(
                message.source,
                "Сначала выбери хотя бы одну карту."
              )
            ).void
          }

        } else {

          store.get(chatId) match {

            case Some(session) =>
              session.state.players.collectFirst { case human: Player.Human =>
                human
              } match {

                case Some(human) =>
                  val cards =
                    selectedIndexes
                      .flatMap(human.hand.lift)
                      .toList

                  val attack =
                    Attack(cards)

                  store.applyAction(
                    id = chatId,
                    action = PlayerAction.AttackCards(attack)
                  ) match {

                    case Right(
                          (
                            updatedStore,
                            SessionResult.Updated(updatedSession)
                          )
                        ) =>
                      // Сначала сохраняем атаку человека
                      store = updatedStore

                      selectedAttackCards = selectedAttackCards - chatId

                      val robot =
                        updatedSession.state.players.collectFirst { case robot: Player.Robot =>
                          robot
                        }

                      robot match {

                        case Some(robotPlayer) =>
                          GameEngine.playRobotDefenseResult(
                            state = updatedSession.state,
                            robot = robotPlayer,
                            attack = attack
                          ) match {

                            case Right(
                              RobotDefenseResult.Defended(
                                stateAfterRobot,
                                defense
                              )
                            ) =>

                              store =
                                store.put(
                                  chatId,
                                  GameSession(stateAfterRobot)
                                )

                              answerCallback
                                .flatMap { _ =>
                                  clearKeyboard(message)
                                }
                                .flatMap { _ =>
                                  request(
                                    SendMessage(
                                      message.source,
                                      s"""Ты атаковал: ${cards.mkString(", ")}
                                         |Робот отбился: ${defense.cards.mkString(", ")}""".stripMargin
                                    )
                                  ).void
                                }
                                .flatMap { _ =>
                                  continueGame(
                                    message = message,
                                    state = stateAfterRobot,
                                    chatId = chatId
                                  )
                                }


                            case Right(
                              RobotDefenseResult.Surrendered(
                                stateAfterRobot
                              )
                            ) =>

                              store =
                                store.put(
                                  chatId,
                                  GameSession(stateAfterRobot)
                                )

                              answerCallback
                                .flatMap { _ =>
                                  clearKeyboard(message)
                                }
                                .flatMap { _ =>
                                  request(
                                    SendMessage(
                                      message.source,
                                      s"""Ты атаковал: ${cards.mkString(", ")}
                                         |Робот не смог отбиться и сдал карты.""".stripMargin
                                    )
                                  ).void
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
                                request(
                                  SendMessage(
                                    message.source,
                                    s"Ошибка хода робота: $error"
                                  )
                                ).void
                              }
                          }

                        case None =>
                          answerCallback.flatMap { _ =>
                            request(
                              SendMessage(
                                message.source,
                                "Robot player not found"
                              )
                            ).void
                          }
                      }

                    case Right(
                          (
                            updatedStore,
                            SessionResult.Finished(game)
                          )
                        ) =>
                      store = updatedStore

                      selectedAttackCards = selectedAttackCards - chatId

                      answerCallback.flatMap { _ =>
                        request(
                          SendMessage(
                            message.source,
                            s"Игра закончена: ${game.result}"
                          )
                        ).void
                      }

                    case Left(error) =>
                      answerCallback.flatMap { _ =>
                        request(
                          SendMessage(
                            message.source,
                            s"Ошибка: $error"
                          )
                        ).void
                      }
                  }

                case None =>
                  answerCallback
              }

            case None =>
              answerCallback.flatMap { _ =>
                request(
                  SendMessage(
                    message.source,
                    "Игра не найдена. Используй /newgame"
                  )
                ).void
              }
          }
        }

      case _ =>
        answerCallback
    }
  }

  private def attackKeyboard(
    player: Player.Human,
    selected: Vector[Int]
  ): InlineKeyboardMarkup = {

    val buttons =
      player.hand.zipWithIndex.map { case (card, index) =>
        val text =
          if (selected.contains(index))
            s"✅ $card"
          else
            card.toString

        InlineKeyboardButton(
          text = text,
          callbackData = Some(s"select-attack:$index")
        )
      }

    val attackButton =
      InlineKeyboardButton(
        text = "Атаковать",
        callbackData = Some("attack:confirm")
      )

    val declare31Button =
      InlineKeyboardButton(
        text = "Объявить 31",
        callbackData = Some("declare:31")
      )

    InlineKeyboardMarkup(
      Seq(
        buttons,
        Seq(
          attackButton,
          declare31Button
        )
      )
    )
  }

  private def defenseKeyboard(
                               player: Player.Human,
                               selected: Vector[Int]
                             ): InlineKeyboardMarkup = {

    val buttons =
      player.hand.zipWithIndex.map {
        case (card, index) =>

          val text =
            if (selected.contains(index))
              s"✅ $card"
            else
              card.toString

          InlineKeyboardButton(
            text = text,
            callbackData = Some(s"select-defense:$index")
          )
      }

    val defendButton =
      InlineKeyboardButton(
        text = "Отбиться",
        callbackData = Some("defense:confirm")
      )

    val surrenderButton =
      InlineKeyboardButton(
        text = "Сдаться",
        callbackData = Some("defense:surrender")
      )

    InlineKeyboardMarkup(
      Seq(
        buttons,
        Seq(defendButton, surrenderButton)
      )
    )
  }

  private def suitSymbol(suit: Suit): String =
    suit match {
      case Suit.Clubs    => "♣️"
      case Suit.Diamonds => "♦️"
      case Suit.Hearts   => "♥️"
      case Suit.Spades   => "♠️"
    }

  private def clearKeyboard(
                             message: Message
                           ): Future[Unit] =
    request(
      EditMessageReplyMarkup(
        chatId = Some(message.source),
        messageId = Some(message.messageId),
        replyMarkup = None
      )
    ).void
}
