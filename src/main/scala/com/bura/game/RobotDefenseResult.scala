package com.bura.game

import com.bura.model.Defense

sealed trait RobotDefenseResult

object RobotDefenseResult {

  case class Defended(state: GameState, defense: Defense) extends RobotDefenseResult

  case class Surrendered(state: GameState) extends RobotDefenseResult
}