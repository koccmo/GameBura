package com.bura.telegram

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object TelegramMain extends App {

  val token =
    sys.env.getOrElse(
      "TELEGRAM_BOT_TOKEN",
      throw new RuntimeException(
        "TELEGRAM_BOT_TOKEN is not set"
      )
    )

  val bot =
    new BuraTelegramBot(token)

  val eol =
    bot.run()

  eol.onComplete {
    case Success(_) =>
      println("Telegram polling stopped normally.")

    case Failure(error) =>
      println(
        s"Telegram polling failed: ${error.getClass.getName}: ${error.getMessage}"
      )

      error.printStackTrace()
  }

  println("Telegram bot started.")
  println("Press Ctrl+C to stop.")

  Await.result(
    eol,
    Duration.Inf
  )
}