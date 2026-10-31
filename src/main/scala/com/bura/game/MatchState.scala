package com.bura.game

case class MatchState(
                       humanWins: Int = 0,
                       robotWins: Int = 0
                     ) {

  def update(result: GameResult): MatchState =
    result match {
      case _: GameResult.HumanWon => copy(humanWins = humanWins + 1)
      case _: GameResult.RobotWon => copy(robotWins = robotWins + 1)
      case _: GameResult.Draw => this
    }
}