package com.bura.telegram

import com.bura.game.GameSessionStore

final class TelegramBotState {

  var store: GameSessionStore = GameSessionStore()

  var selectedAttackCards: Map[Long, Vector[Int]] = Map.empty

  var selectedDefenseCards: Map[Long, Vector[Int]] = Map.empty

  var waitingForName: Set[Long] = Set.empty
}