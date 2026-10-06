package com.bura.rules
import com.bura.model.Rank.{Ace, Eight, Jack, King, Nine, Queen, Seven, Six, Ten}
import com.bura.model.Suit.{Hearts, Spades}
import com.bura.model._
import org.scalatest.funsuite.AnyFunSuite

class RulesSpec extends AnyFunSuite {

  test("higher card of the same suit beats lower card") {
    val attackCard = Card(Six, Hearts)
    val defenseCard = Card(Jack, Hearts)
    val result = Rules.beatsOne(attackCard, defenseCard, Spades)

    assert(result)
  }

  test("lower card of the same suit does not beat higher card") {
    val attackCard = Card(Jack, Hearts)
    val defenseCard = Card(Nine, Hearts)
    val result = Rules.beatsOne(attackCard, defenseCard, Spades)

    assert(!result)
  }

  test("trump beats card of another suit") {
    val attackCard = Card(Ace, Hearts)
    val defenseCard = Card(Six, Spades)
    val trump = Spades
    val result = Rules.beatsOne(attackCard, defenseCard, trump)

    assert(result)
  }

  test("non-trump card does not beat trump") {
    val attackCard = Card(Six, Spades)
    val defenseCard = Card(Ace, Hearts)
    val trump = Spades
    val result = Rules.beatsOne(attackCard, defenseCard, trump)

    assert(!result)
  }

  test("defense can beat multiple attack cards using permutations") {
    val attack = Attack(List(Card(Seven, Hearts), Card(Ten, Hearts)))
    val defense = Defense(List(Card(Ace, Hearts), Card(Eight, Hearts)))
    val trump = Spades
    val result = Rules.beatsAttack(attack, defense, trump)

    assert(result)
  }

  test("defense must contain the same number of cards as attack") {
    val attack = Attack(List(Card(Seven, Hearts), Card(Eight, Hearts)))
    val defense = Defense(List(Card(Ten, Hearts)))
    val trump = Spades
    val result = Rules.beatsAttack(attack, defense, trump)

    assert(!result)
  }

  test("defense fails if no permutation beats all attack cards") {

    val attack = Attack(List(Card(Queen, Hearts), Card(Ace, Hearts)))
    val defense = Defense(List(Card(King, Hearts), Card(Ten, Hearts)))
    val trump = Suit.Spades
    val result = Rules.beatsAttack(attack, defense, trump)

    assert(!result)
  }

  test("three defense cards can beat three attack cards") {

    val attack = Attack(List(Card(Seven, Hearts), Card(Nine, Hearts), Card(Ten, Hearts)))
    val defense = Defense(List(Card(Eight, Hearts), Card(Ten, Hearts), Card(Ace, Hearts)))
    val trump = Suit.Spades
    val result = Rules.beatsAttack(attack, defense, trump)

    assert(result)
  }

  test("three defense cards can beat attack using another permutation") {

    val attack = Attack(List(Card(Seven, Hearts), Card(Queen, Hearts), Card(Ten, Hearts)))
    val defense = Defense(List(Card(Ace, Hearts), Card(Eight, Hearts), Card(King, Hearts)))
    val trump = Suit.Spades
    val result = Rules.beatsAttack(attack, defense, trump)

    assert(result)
  }

  test("same suit defense card must be higher than attack card") {

    val attack = Attack(List(Card(King, Hearts)))
    val defense = Defense(List(Card(Queen, Hearts)))
    val result = Rules.beatsAttack(attack, defense, Spades)

    assert(!result)
  }

  test("trump defense card beats higher non-trump attack card") {

    val attack = Attack(List(Card(Ace, Hearts)))
    val defense = Defense(List(Card(Six, Spades)))
    val result = Rules.beatsAttack(attack, defense, Spades)

    assert(result)
  }
}
