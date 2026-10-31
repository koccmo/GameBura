package com.bura.telegram

import com.bot4s.telegram.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import com.bura.model.Player

object TelegramKeyboards {

  def playAgainKeyboard: InlineKeyboardMarkup =
    InlineKeyboardMarkup(
      Seq(
        Seq(
          InlineKeyboardButton(text = "🔄 Play Again", callbackData = Some("game:start"))
        )
      )
    )

  def backToMenuKeyboard: InlineKeyboardMarkup =
    InlineKeyboardMarkup(
      Seq(
        Seq(
          InlineKeyboardButton(
            text = "🏠 Main Menu",
            callbackData = Some("menu:main")
          )
        )
      )
    )

  def mainMenuKeyboard: InlineKeyboardMarkup =
    InlineKeyboardMarkup(
      Seq(
        Seq(
          InlineKeyboardButton(text = "🎮 Start", callbackData = Some("game:start"))),
        Seq(
          InlineKeyboardButton(text = "📖 Rules", callbackData = Some("menu:rules")),
          InlineKeyboardButton(text = "🧮 Cards Points", callbackData = Some("menu:points"))
        )
      )
    )

  def attackKeyboard(
    player: Player.Human,
    selected: Vector[Int]
  ): InlineKeyboardMarkup = {

    val buttons: List[InlineKeyboardButton] =
      player.hand.zipWithIndex.map { case (card, index) =>
        val text =
          if (selected.contains(index)) s"✅ $card"
          else card.toString

        InlineKeyboardButton(text = text, callbackData = Some(s"select-attack:$index"))
      }

    val attackButton = InlineKeyboardButton(text = "Attack", callbackData = Some("attack:confirm"))

    val declare31Button = InlineKeyboardButton(text = "Declare 31", callbackData = Some("declare:31"))

    InlineKeyboardMarkup(
      Seq(buttons, Seq(attackButton, declare31Button))
    )
  }

  def defenseKeyboard(
    player: Player.Human,
    selected: Vector[Int]
  ): InlineKeyboardMarkup = {

    val buttons: List[InlineKeyboardButton] =
      player.hand.zipWithIndex.map { case (card, index) =>
        val text =
          if (selected.contains(index)) s"✅ $card"
          else card.toString

        InlineKeyboardButton(text = text, callbackData = Some(s"select-defense:$index"))
      }

    val defendButton =
      InlineKeyboardButton(text = "Defense", callbackData = Some("defense:confirm"))

    val surrenderButton =
      InlineKeyboardButton(text = "Surrender", callbackData = Some("defense:surrender"))

    InlineKeyboardMarkup(
      Seq(
        buttons,
        Seq(defendButton, surrenderButton)
      )
    )
  }
}
