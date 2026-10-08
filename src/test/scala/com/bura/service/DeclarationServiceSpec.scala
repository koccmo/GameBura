package com.bura.service

import com.bura.game.GameResult
import org.scalatest.funsuite.AnyFunSuite

class DeclarationServiceSpec extends AnyFunSuite {

  test("humanDeclarationResult returns HumanWon when human has enough points") {

    val result = DeclarationService.humanDeclarationResult(humanPoints = 31, robotPoints = 20)

    assert(result == GameResult.HumanWon(humanPoints = 31, robotPoints = 20))
  }

  test("humanDeclarationResult returns RobotWon when human has less than 31 points") {

    val result = DeclarationService.humanDeclarationResult(humanPoints = 30, robotPoints = 20)

    assert(result == GameResult.RobotWon(humanPoints = 30, robotPoints = 20))
  }

  test("robotDeclarationResult returns RobotWon when robot has enough points") {

    val result = DeclarationService.robotDeclarationResult(humanPoints = 20, robotPoints = 31)

    assert(result == GameResult.RobotWon(humanPoints = 20, robotPoints = 31))
  }

  test("robotDeclarationResult returns HumanWon when robot has less than 31 points") {

    val result = DeclarationService.robotDeclarationResult(humanPoints = 20, robotPoints = 30)

    assert(result == GameResult.HumanWon(humanPoints = 20, robotPoints = 30))
  }

  

}
