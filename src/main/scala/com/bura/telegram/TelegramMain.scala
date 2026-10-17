package com.bura.telegram

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration

object TelegramMain extends App {

  val token = sys.env.getOrElse("TELEGRAM_BOT_TOKEN", throw new RuntimeException("TELEGRAM_BOT_TOKEN is not set"))

  val bot = new BuraTelegramBot(token)

  val eol: Future[Unit] = bot.run()

  println("Telegram bot started.")
  println("Press ENTER to stop.")

  scala.io.StdIn.readLine()

  bot.shutdown()

  Await.result(eol, Duration.Inf)
}
