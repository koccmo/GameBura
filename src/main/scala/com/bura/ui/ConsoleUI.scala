package com.bura.ui

import com.bura.game.{GameResult, GameState}
import com.bura.model.{Attack, Card, Defense, Player}
import com.bura.rules.BuraRules
import scala.io.StdIn.readLine


object ConsoleUI {

  def chooseAttack(state: GameState, hand: List[Card]): Either[String, Attack] = {
    showGameInfo(state)
    println(s"${state.attackingPlayer.name} attacks:")
    chooseCards(hand, minAmount = 1, maxAmount = BuraRules.MaxAttackCards).map(Attack(_))
  }

  def chooseDefense(hand: List[Card], amount: Int): Either[String, Defense] =
    chooseCards(hand = hand, minAmount = amount, maxAmount = amount, showHandBefore = false).map(Defense(_))

  def chooseSurrender(hand: List[Card], amount: Int): Either[String, Defense] =
    chooseCards(hand = hand, minAmount = amount, maxAmount = amount, showHandBefore = false).map(Defense(_))

  def chooseDefenseAction(state: GameState, attack: Attack, hand: List[Card]): Either[String, DefenseAction] = {
    println(s"Trump: ${state.trump}")
    println(s"${state.attackingPlayer.name} attacks:")

    attack.cards.zipWithIndex.foreach {
      case (card, index) => println(s"${index + 1}- $card")
    }

    println("Your hand:")

    hand.zipWithIndex.foreach {
      case (card, index) => println(s"${index + 1}- $card")
    }
    println()
    println("1 - Defend")
    println("2 - Surrender")
    print("Choose action: ")

    readLine().trim match {
      case "1" => Right(DefenseAction.Defend)
      case "2" => Right(DefenseAction.Surrender)
      case _ => Left("Invalid action")
    }
  }

  def showRoundResult(state: GameState): Unit = {

    val winner = state.attackingPlayer
    println()
    println(s"${winner.name} wins the trick!")

    showScore(state)
  }

  def showGameResult(result: GameResult): Unit = {

    println()
    println("===== GAME OVER =====")
    println()

    result match {
      case GameResult.HumanWon(humanPoints, robotPoints) =>
        println("Human wins!")
        println(s"Human: $humanPoints")
        println(s"Robot: $robotPoints")
      case GameResult.RobotWon(humanPoints, robotPoints) =>
        println("Robot wins!")
        println(s"Human: $humanPoints")
        println(s"Robot: $robotPoints")
      case GameResult.Draw(humanPoints, robotPoints) =>
        println("Draw!")
        println(s"Human: $humanPoints")
        println(s"Robot: $robotPoints")
    }
  }

  def chooseDeclareAction(player: Player): Either[String, DeclareAction] = {

    println(s"Visible points: ${ player.trickPoints }")
    println(s"Hidden cards: ${ player.hiddenTricks.map(_.count).sum }")
    println("1 - Continue")
    println(s"2 - Declare ${BuraRules.WinningPoints}")
    print("Choose action: ")

    readLine().trim match {

      case "1" =>
        Right(DeclareAction.Continue)

      case "2" =>
        Right(DeclareAction.Declare31)

      case _ =>
        Left("Invalid action")
    }
  }

  private def showScore(state: GameState): Unit = {
    println()
    println("Score:")
    state.players.foreach{ player =>
      println(s"${ player.name }: ${ player.trickPoints }")
    }
    println()
  }

  private def showGameInfo(state: GameState): Unit = {
    println(s"Trump: ${ state.trump }")
  }

  private def showHand(hand: List[Card]): Unit = {
    println("Your hand:")

    hand.zipWithIndex.foreach {
      case (card, index) => println(s"${index + 1}. $card")
    }
  }

  private def showAttack(attack: Attack): Unit = {
    println()
    println("Opponent attacks:")
    attack.cards.zipWithIndex.foreach{
      case (card, index) => println(s"${index + 1}. $card")
    }
  }

  private def chooseCards(
                           hand: List[Card],
                           minAmount: Int,
                           maxAmount: Int,
                           showHandBefore: Boolean = true
                         ): Either[String, List[Card]] = {

    if (showHandBefore) { showHand(hand) }

    val message =
      if (minAmount == maxAmount) s"Choose $minAmount card(s): "
      else s"Choose $minAmount-$maxAmount card(s): "

    print(message)

    parseCardIndexes(
      input = readLine(),
      hand = hand,
      minAmount = minAmount,
      maxAmount = maxAmount
    )
  }

  private def parseCardIndexes(
                                input: String,
                                hand: List[Card],
                                minAmount: Int,
                                maxAmount: Int): Either[String, List[Card]] = {

    val compactInput =
      input.filterNot(_.isWhitespace)

    if (compactInput.isEmpty || !compactInput.forall(_.isDigit)) {
      Left("Invalid input")}
    else { val indexes =
        compactInput
          .toList
          .map(_.asDigit)

      if (indexes.size < minAmount || indexes.size > maxAmount) {
        Left(s"You must choose from $minAmount to $maxAmount card(s)")}
      else if (indexes.distinct.size != indexes.size) {
        Left("You cannot choose the same card twice")}
      else if (indexes.exists(index => index < 1 || index > hand.size)) {
        Left("Invalid card number")}
      else { Right(indexes.map(index => hand(index - 1))) }
    }
  }


}
