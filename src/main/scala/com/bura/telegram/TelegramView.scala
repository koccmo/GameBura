package com.bura.telegram

import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.{InlineKeyboardMarkup, Message}
import com.bot4s.telegram.methods.{EditMessageReplyMarkup, SendMessage}

import scala.concurrent.Future

final class TelegramView(
                          sendMessageRequest: SendMessage => Future[Unit],
                          editReplyMarkupRequest:
                          EditMessageReplyMarkup => Future[Unit]
                        ) {

  def sendMessage(message: Message, text: String, keyboard: Option[InlineKeyboardMarkup]): Future[Unit] =
    sendMessageRequest(SendMessage(message.source, text, replyMarkup = keyboard))

  def clearKeyboard(message: Message): Future[Unit] =
    editReplyMarkupRequest(
      EditMessageReplyMarkup(chatId = Some(message.source), messageId = Some(message.messageId), replyMarkup = None)
    )

  def updateKeyboard(message: Message, keyboard: InlineKeyboardMarkup): Future[Unit] =
    editReplyMarkupRequest(
      EditMessageReplyMarkup(
        chatId = Some(message.source),
        messageId = Some(message.messageId),
        replyMarkup = Some(keyboard)
      )
    )
}