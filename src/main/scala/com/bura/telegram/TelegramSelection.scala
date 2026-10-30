package com.bura.telegram

object TelegramSelection {

  def toggle(currentSelection: Vector[Int], index: Int, maxCards: Int = 3): Vector[Int] = {

    if (currentSelection.contains(index)) { currentSelection.filterNot(_ == index) }
    else if (currentSelection.size < maxCards) { currentSelection :+ index }
    else { currentSelection }
  }
}