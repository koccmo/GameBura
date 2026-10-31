package com.bura.telegram

import com.bot4s.telegram.models.{InlineKeyboardMarkup, Message}
import scala.concurrent.{ExecutionContext, Future}
import TelegramKeyboards._
import TelegramMessages._
import com.bura.model.Player
import TelegramSelection._

final class TelegramCallbackHandler(botState: TelegramBotState)(implicit ec: ExecutionContext) {

  def isMenuCallback(data: String): Boolean =
    Set(
      "menu:rules",
      "menu:points",
      "menu:main",
      "game:start"
    ).contains(data)

  def handleMenuCallback(
                          message: Message,
                          data: String,
                          answerCallback: Future[Unit],
                          clearKeyboard: Message => Future[Unit],
                          sendMessage: (Message, String, Option[InlineKeyboardMarkup]) => Future[Unit]
                        ): Future[Unit] =
    data match {

      case "menu:rules" =>
        answerCallback
          .flatMap { _ => clearKeyboard(message) }
          .flatMap { _ => sendMessage(message, rules, Some(backToMenuKeyboard)) }

      case "menu:points" =>
        answerCallback
          .flatMap { _ =>clearKeyboard(message) }
          .flatMap { _ =>sendMessage(message, cardPoints, Some(backToMenuKeyboard)) }

      case "menu:main" =>
        answerCallback
          .flatMap { _ => clearKeyboard(message) }
          .flatMap { _ => sendMessage(message, mainMenu, Some(mainMenuKeyboard)) }

      case "game:start" =>
        val chatId = message.chat.id

        botState.waitingForName = botState.waitingForName + chatId

        botState.selectedAttackCards = botState.selectedAttackCards - chatId

        botState.selectedDefenseCards = botState.selectedDefenseCards - chatId

        answerCallback
          .flatMap { _ => clearKeyboard(message) }
          .flatMap { _ => sendMessage(message, newGamePrompt, None) }

      case _ => answerCallback
    }

  def handleCardSelection(
                           message: Message,
                           data: String,
                           prefix: String,
                           selections: Map[Long, Vector[Int]],
                           updateSelections: Map[Long, Vector[Int]] => Unit,
                           keyboardBuilder: (Player.Human, Vector[Int]) => InlineKeyboardMarkup,
                           answerCallback: Future[Unit],
                           updateKeyboard: (Message, InlineKeyboardMarkup) => Future[Unit]
                         ): Future[Unit] = {

    val chatId = message.chat.id

    val indexOption = data.stripPrefix(prefix).toIntOption

    indexOption match {

      case Some(index) =>

        val currentSelection = selections.getOrElse(chatId, Vector.empty)

        val newSelection = toggle(currentSelection = currentSelection, index = index)

        val updatedSelections = selections.updated(chatId, newSelection)

        updateSelections(updatedSelections)

        answerCallback.flatMap { _ => botState.store.get(chatId) match {

            case Some(session) =>
              val human = session.state.players.collectFirst { case human: Player.Human => human}

              human match {
                case Some(player) =>
                  val keyboard = keyboardBuilder(player, newSelection)
                  updateKeyboard(message, keyboard)

                case None => Future.successful(())
              }

            case None => Future.successful(())
          }
        }

      case None => answerCallback
    }
  }
}