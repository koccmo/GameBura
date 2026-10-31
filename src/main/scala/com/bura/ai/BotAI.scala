package com.bura.ai

import com.bura.game.GameState
import com.bura.model.{Attack, Defense, Player}
import com.bura.rules.BuraRules

object BotAi {
  def chooseAttack(state: GameState, robot: Player.Robot): Option[Attack] = {
    BotAttack.choose(state, robot)
  }

  def chooseDefense(state: GameState, robot: Player.Robot, attack: Attack): Option[Defense] = {
    BotDefense.choose(state, robot, attack)
  }

  def chooseSurrenderDefense(state: GameState, robot: Player.Robot, attack: Attack): Option[Defense] =
    BotDefense.chooseSurrenderDefense(state, robot, attack)

  def shouldDeclare31(robot: Player.Robot): Boolean = {

    val visiblePoints = robot.trickPoints
    val hiddenCardsAmount = robot.hiddenTricks.map(_.count).sum

    if (visiblePoints >= BuraRules.WinningPoints) true
    else if (visiblePoints >= 25 && hiddenCardsAmount >= 6) true
    else if (visiblePoints >= 20 && hiddenCardsAmount >= 8) true
    else false
  }
}
