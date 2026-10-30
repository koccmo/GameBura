package com.bura.telegram

import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.{AnswerCallbackQuery, SendMessage}
import com.bot4s.telegram.models.{CallbackQuery, Message}

import com.bura.game.PlayerAction
import io.circe.Decoder

import sttp.client4.Backend
import sttp.client4.okhttp.OkHttpFutureBackend

import scala.concurrent.Future

import TelegramKeyboards._
import TelegramMessages._

import cats.syntax.functor._
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.clients.FutureSttpClient


class BuraTelegramBot(token: String) extends TelegramBot with Polling {

  implicit val editMessageResponseDecoder: Decoder[Either[Boolean, Message]] =
    Decoder[Boolean].either(Decoder[Message])

  private val botState = new TelegramBotState
  private val callbackHandler = new TelegramCallbackHandler(botState)
  private val telegramView =
    new TelegramView(
      sendMessageRequest = sendMessage => request(sendMessage).void,
      editReplyMarkupRequest = edit => request(edit).void
    )

  private val gameFlow = new TelegramGameFlow(botState = botState, telegramView = telegramView)

  implicit val backend: Backend[Future] = OkHttpFutureBackend()

  override val client: RequestHandler[Future] = new FutureSttpClient(token)

  override def receiveMessage(msg: Message): Future[Unit] = {

    val chatId = msg.chat.id

    msg.text match {

      // User Set Name
      case Some(text)
        if botState.waitingForName.contains(chatId) && !text.startsWith("/") => val humanName = text.trim

        if (humanName.isEmpty) {

          request(SendMessage(msg.source, emptyName)).void

        } else {

          botState.store.createGame(id = chatId, humanName = humanName) match {

            case Right(updatedStore) =>
              botState.store = updatedStore

              botState.waitingForName = botState.waitingForName - chatId

              botState.selectedAttackCards = botState.selectedAttackCards - chatId

              botState.selectedDefenseCards = botState.selectedDefenseCards - chatId

              botState.store.get(chatId) match {

                case Some(session) =>
                  request(SendMessage(msg.source, gameStarted(humanName))).void.flatMap { _ =>
                    gameFlow.sendState(message = msg, session = session)
                  }

                case None => Future.successful(())
              }

            case Left(error) => request(SendMessage(msg.source, gameCreationError(error))).void
          }
        }

      // START
      case Some("/start") =>
        request(SendMessage(msg.source, mainMenu, replyMarkup = Some(mainMenuKeyboard))).void

      // HELP
      case Some("/help")  => request(SendMessage(msg.source, help)).void

      case _ => Future.successful(())
    }
  }

  override def receiveCallbackQuery(callbackQuery: CallbackQuery): Future[Unit] = {

    val answerCallback = request(AnswerCallbackQuery(callbackQuery.id)).void

    (callbackQuery.message, callbackQuery.data) match {

      case (Some(message), Some(data))
        if callbackHandler.isMenuCallback(data) =>

        callbackHandler.handleMenuCallback(
          message = message,
          data = data,
          answerCallback = answerCallback,
          clearKeyboard = telegramView.clearKeyboard,
          sendMessage = telegramView.sendMessage
        )

      // Select-Attack
      case (Some(message), Some(data))
        if data.startsWith("select-attack:") =>

        callbackHandler.handleCardSelection(
          message = message,
          data = data,
          prefix = "select-attack:",
          selections = botState.selectedAttackCards,
          updateSelections = updated => botState.selectedAttackCards = updated,
          keyboardBuilder = attackKeyboard,
          answerCallback = answerCallback,
          updateKeyboard = telegramView.updateKeyboard
        )

      //Select-Defense
      case (Some(message), Some(data))
        if data.startsWith("select-defense:") =>

        callbackHandler.handleCardSelection(
          message = message,
          data = data,
          prefix = "select-defense:",
          selections = botState.selectedDefenseCards,
          updateSelections = updated => botState.selectedDefenseCards = updated,
          keyboardBuilder = defenseKeyboard,
          answerCallback = answerCallback,
          updateKeyboard = telegramView.updateKeyboard
        )

      //Button Defense
      case (Some(message), Some("defense:confirm")) =>

        gameFlow.handleDefenseAction(
          message = message,
          chatId = message.chat.id,
          answerCallback = answerCallback,
          emptySelectionMessage = selectDefenseCards,
          invalidRoundMessage = cannotDefendNow,
          actionBuilder = PlayerAction.DefendCards.apply,
          successMessage = cards => defended(cards.mkString(", ")),
          actionErrorMessage = defenseError
        )

      //Button  «Surrend»
      case (Some(message), Some("defense:surrender")) =>

        gameFlow.handleDefenseAction(
          message = message,
          chatId = message.chat.id,
          answerCallback = answerCallback,
          emptySelectionMessage = selectSurrenderCards,
          invalidRoundMessage = cannotSurrenderNow,
          actionBuilder = PlayerAction.SurrenderCards.apply,
          successMessage = cards => surrendered(cards.size),
          actionErrorMessage = surrenderError
        )

      //Declare 31
      case (Some(message), Some("declare:31")) =>

        gameFlow.handleDeclare31(
          message = message,
          chatId = message.chat.id,
          answerCallback = answerCallback
        )

      // Confirm Attack
      case (Some(message), Some("attack:confirm")) =>

        gameFlow.handleAttackAction(
          message = message,
          chatId = message.chat.id,
          answerCallback = answerCallback
        )

      case _ => answerCallback
    }
  }

}
