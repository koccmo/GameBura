package com.bura

import com.bura.game.{GameEngine, GameFactory}
import com.bura.ui.{ConsoleGameRunner, ConsoleUI}

object Main {

  def main(args: Array[String]): Unit = {

    GameFactory.create() match {

      case Left(error) => println(s"Cannot start game: $error")
      case Right(initialState) => ConsoleGameRunner.play(initialState) match {
          case Left(error) => println(s"Game error: $error")
          case Right(finishedGame) => ConsoleUI.showGameResult(finishedGame.result)
        }
    }
  }
}
