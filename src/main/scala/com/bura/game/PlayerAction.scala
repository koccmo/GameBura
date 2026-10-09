package com.bura.game

import com.bura.model.{Attack, Defense}

sealed trait PlayerAction

object PlayerAction {

  case class AttackCards(attack: Attack) extends PlayerAction

  case class DefendCards(defense: Defense) extends PlayerAction

  case class SurrenderCards(defense: Defense) extends PlayerAction

  case object Declare31 extends PlayerAction

  case object Continue extends PlayerAction
}
