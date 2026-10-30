package com.bura.telegram

object TelegramMessages {

  def gameState(
                 trump: String,
                 attacker: String,
                 defender: String,
                 humanPoints: Int,
                 humanCards: String
               ): String =
    s"""Trump: $trump
       |Attacker: $attacker
       |Defender: $defender
       |Your points: $humanPoints
       |Your cards: $humanCards""".stripMargin



  val mainMenu: String =
    """🃏 BURA
      |
      |━━━━━━━━━━━━━━
      |
      |Welcome to the game!
      |
      |Choose an action below.""".stripMargin

  val help: String =
    """HOW TO PLAY
      |
      |━━━━━━━━━━━━━━
      |
      |• Select 1–3 cards of the same suit and press "Attack".
      |• If the robot attacks, select your cards and press "Defend".
      |• If you don't want to defend, select the required number of cards and press "Surrender".
      |• If you have 31 or more points, you can declare 31.""".stripMargin

  val rules: String =
    """📖 GAME RULES
      |
      |━━━━━━━━━━━━━━
      |🎴 ATTACK
      |
      |• You can attack with 1–3 cards.
      |• All attack cards must have the same suit.
      |
      |━━━━━━━━━━━━━━
      |🛡 DEFENSE
      |
      |• You must play the same number of cards as the attacker.
      |• A higher card of the same suit beats a lower card.
      |• A trump card beats a card of another suit.
      |
      |━━━━━━━━━━━━━━
      |🏳 SURRENDER
      |
      |• You may choose not to defend.
      |• You must surrender the same number of cards as were played in the attack.
      |• These cards are placed face down.
      |
      |━━━━━━━━━━━━━━
      |🏆 DECLARE 31
      |
      |• If you have 31 or more points, you may declare 31.
      |• If you declare 31 too early, you lose.""".stripMargin

  val cardPoints: String =
    """🧮 CARD POINTS
      |
      |━━━━━━━━━━━━━━
      |
      |A — 11 points
      |10 — 10 points
      |K — 4 points
      |Q — 3 points
      |J — 2 points
      |9 — 0 points
      |8 — 0 points
      |7 — 0 points
      |6 — 0 points
      |
      |━━━━━━━━━━━━━━
      |
      |Total points in the deck: 120.""".stripMargin

  val newGamePrompt: String =
    """👤 NEW GAME
      |
      |━━━━━━━━━━━━━━
      |
      |What's your name?
      |
      |Enter your name in the next message.""".stripMargin

  val emptyName: String = "Your name cannot be empty. Please enter your name."

  def gameStarted(name: String): String = s"🎮 The game starts, $name!"

  def gameCreationError(error: String): String = s"Could not create the game: $error"

  val gameNotStarted: String = "The game has not started yet."

  val gameNotFound: String = "Game not found. Start a new game from the main menu."

  def robotAttack(cards: String): String =
    s"Robot attacks: $cards"

  def robotAttackError(error: String): String =
    s"Robot attack error: $error"

  val unexpectedRobotAttackState: String = "Error: defense was expected after the robot's attack."

  val selectDefenseCards: String = "Select cards for defense first."

  def defended(cards: String): String = s"You defended with: $cards"

  def defenseError(error: String): String = s"Defense error: $error"

  val cannotDefendNow: String = "You cannot defend right now."

  val selectSurrenderCards: String = "Select the cards you want to surrender first."

  def surrendered(cardCount: Int): String =
    cardCount match {
      case 1 => "You surrendered 1 card."
      case n => s"You surrendered $n cards."
    }

  def surrenderError(error: String): String = s"Surrender error: $error"

  val cannotSurrenderNow: String = "You cannot surrender right now."

  val selectAttackCards: String = "Select at least one card first."

  def humanAttackRobotDefended(attackCards: String, defenseCards: String): String =
    s"""You attacked with: $attackCards
       |Robot defended with: $defenseCards""".stripMargin

  def humanAttackRobotSurrendered(attackCards: String): String =
    s"""You attacked with: $attackCards
       |Robot could not defend and surrendered.""".stripMargin

  def robotTurnError(error: String): String = s"Robot turn error: $error"

  val robotNotFound: String = "Robot player not found."

  def attackError(error: String): String =
    s"Attack error: $error"

  def humanWon(
                humanPoints: Int,
                robotPoints: Int
              ): String =
    s"""🏆 You won!
       |Your points: $humanPoints
       |Robot points: $robotPoints""".stripMargin

  def robotWon(
                humanPoints: Int,
                robotPoints: Int
              ): String =
    s"""🤖 Robot won.
       |Your points: $humanPoints
       |Robot points: $robotPoints""".stripMargin

  def draw(
            humanPoints: Int,
            robotPoints: Int
          ): String =
    s"""🤝 Draw.
       |Your points: $humanPoints
       |Robot points: $robotPoints""".stripMargin

  def humanDeclared31Won(
                          humanPoints: Int,
                          robotPoints: Int
                        ): String =
    s"""🏆 You declared 31 and won!
       |Your points: $humanPoints
       |Robot points: $robotPoints""".stripMargin

  def humanDeclared31Lost(
                           humanPoints: Int,
                           robotPoints: Int
                         ): String =
    s"""❌ You declared 31 too early.
       |Robot won.
       |Your points: $humanPoints
       |Robot points: $robotPoints""".stripMargin

  def robotDeclared31Won(
                          humanPoints: Int,
                          robotPoints: Int
                        ): String =
    s"""🤖 Robot declared 31 and won!
       |Your points: $humanPoints
       |Robot points: $robotPoints""".stripMargin

  def robotDeclared31Lost(
                           humanPoints: Int,
                           robotPoints: Int
                         ): String =
    s"""🏆 Robot declared 31 too early.
       |You won!
       |Your points: $humanPoints
       |Robot points: $robotPoints""".stripMargin

  def declarationError(error: String): String =
    s"Declare 31 error: $error"

  def gameResultError(error: String): String =
    s"Could not determine the game result: $error"

  def gameFinished(result: String): String =
    s"Game finished: $result"
}