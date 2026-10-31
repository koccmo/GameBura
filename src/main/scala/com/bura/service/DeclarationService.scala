package com.bura.service

import com.bura.game.GameResult
import com.bura.rules.BuraRules

object DeclarationService {

  def humanDeclarationResult(humanPoints: Int, robotPoints: Int): GameResult =
    if (humanPoints >= BuraRules.WinningPoints)
      GameResult.HumanWon(humanPoints = humanPoints, robotPoints = robotPoints)
    else
      GameResult.RobotWon(humanPoints = humanPoints, robotPoints = robotPoints)

  def robotDeclarationResult(humanPoints: Int, robotPoints: Int): GameResult =
    if (robotPoints >= BuraRules.WinningPoints)
      GameResult.RobotWon(humanPoints = humanPoints, robotPoints = robotPoints)
    else
      GameResult.HumanWon(humanPoints = humanPoints, robotPoints = robotPoints)

}
