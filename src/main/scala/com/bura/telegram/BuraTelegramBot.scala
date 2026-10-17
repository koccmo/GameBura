package com.bura.telegram

import cats.syntax.functor._

import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.clients.FutureSttpClient
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.Message

import sttp.client4.Backend
import sttp.client4.okhttp.OkHttpFutureBackend

import scala.concurrent.Future

class BuraTelegramBot(token: String) extends TelegramBot with Polling {

  implicit val backend: Backend[Future] = OkHttpFutureBackend()

  override val client: RequestHandler[Future] = new FutureSttpClient(token)

  override def receiveMessage(msg: Message): Future[Unit] =
    msg.text match {

      case Some("/start") => request(SendMessage(msg.source, "Привет! Я бот для игры в Буру.")).void

      case _ => Future.successful(())
    }
}
