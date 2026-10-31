# Bura Game

Bura is a small card game project written in Scala.

I started this project mainly to practice Scala and gradually turned it into a complete playable game with game rules, a computer opponent, tests, a console version, and a Telegram bot.

The project also helped me practice working with immutable data, ADTs, `Option`, `Either`, pattern matching, collections, `Future`, and separating application logic into smaller components.

## What is implemented

The game can be played against a bot.

Main features:

- 36-card deck
- 3 cards per player
- attack with 1–3 cards of the same suit
- defense with higher cards of the same suit or trump cards
- surrender when a player cannot or does not want to defend
- trump suit
- card points and trick counting
- declaration of 31 points
- automatic robot turns
- game result calculation
- console version
- Telegram bot interface
- automated tests

## Telegram bot

The Telegram version allows the player to play the whole game using inline buttons.

The bot supports:

- starting a new game
- entering the player's name
- selecting attack cards
- selecting defense cards
- defending or surrendering
- declaring 31
- automatic robot moves
- displaying the current hand and game state
- showing the final result
- starting another game after the previous one finishes
- rules and card point information

The Telegram part was later refactored into smaller components instead of keeping all the logic inside one large bot class.

For example:

```text
telegram/
├── BuraTelegramBot.scala
├── TelegramBotState.scala
├── TelegramCallbackHandler.scala
├── TelegramGameFlow.scala
├── TelegramKeyboards.scala
├── TelegramMessages.scala
├── TelegramSelection.scala
├── TelegramView.scala
└── TelegramMain.scala
```

`BuraTelegramBot` mainly receives Telegram events and routes them to the appropriate part of the application.

`TelegramGameFlow` contains the game flow used by Telegram.

`TelegramView` is responsible for sending messages and updating Telegram keyboards.

`TelegramCallbackHandler` handles callback button actions and card selection.

## Project structure

The main packages are:

```text
com.bura
├── ai
├── game
├── model
├── rules
├── service
├── telegram
└── ui
```

### `model`

Contains the main domain objects such as cards, suits, ranks, attacks, defenses, players, and the deck.

### `rules`

Contains the rules used to determine whether cards can beat an attack.

### `game`

Contains the game state and game flow.

Some examples:

- `GameState`
- `GameEngine`
- `GameSession`
- `GameSessionStore`
- `PlayerAction`
- `GameResult`

### `service`

Contains game actions and declaration logic.

### `ai`

Contains the robot strategy.

The attack and defense logic are separated into:

```text
BotAttack
BotDefense
```

The robot tries to choose valid moves while also considering card points, suits, and trump cards.

### `telegram`

Contains the Telegram interface.

### `ui`

Contains the console version of the game.

## Technologies

The project uses:

- Scala 2.13
- sbt
- ScalaTest
- bot4s
- STTP
- Circe
- Telegram Bot API

## Tests

The project currently has **112 automated tests**.

They cover different parts of the game, including:

- deck operations
- player actions
- game rules
- game service
- game engine
- game sessions
- session storage
- declarations
- robot attacks
- robot defense
- surrender logic
- game results

Run the full test suite with:

```bash
sbt testFull
```

At the current version:

```text
112 tests passed
```

## Running the project

Clone the repository:

```bash
git clone https://github.com/koccmo/GameBura.git
cd GameBura
```

Start sbt:

```bash
sbt
```

The project contains both a console entry point and a Telegram entry point.

For the Telegram version, a valid Telegram Bot API token is required. The token should not be committed to the repository.

## Why I made this project

This started as a learning project while I was studying Scala.

Instead of building many small unrelated exercises, I wanted to keep one project and improve it step by step.

While working on Bura I practiced not only Scala syntax, but also things that become important when a project grows:

- splitting responsibilities between classes
- keeping game rules separate from the user interface
- working with immutable state
- handling errors with `Either` and `Option`
- using pattern matching
- writing tests before changing existing behaviour
- refactoring large methods into smaller components
- integrating application logic with a Telegram bot

There are still things that could be improved, but the current version is fully playable and the main game flow is covered by automated tests.
