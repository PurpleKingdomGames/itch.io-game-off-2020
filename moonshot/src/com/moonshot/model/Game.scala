package com.moonshot.model

import indigo._
// import indigo.shared.events.KeyboardEvent.KeyDown
import indigo.shared.events.KeyboardEvent.KeyUp
import indigo.shared.time.GameTime
import indigoextras.geometry.BoundingBox

final case class Game(gameState: GameState, ship: Ship, timeRemainingInSeconds: Double, asteroids: List[Asteroid], verticalOffset: Double, nextAsteroidSpawn: Double) {
  val verticalSpeed: Double = 40
  // val boundingBox: BoundingBox     = BoundingBox(Vertex(0, 0), Vertex(350, 700))
  val maxAsteroids: Int            = 20
  val minAsteroidSpawnRate: Double = 1
  val maxAsteroidSpawnRate: Double = 3
  val targetVerticalOffset: Double = (Game.maxTimeLimit - 120) * verticalSpeed

  def update(gameTime: GameTime, dice: Dice, shipControl: ShipControl, screenBounds: BoundingBox): GlobalEvent => Outcome[Game] = {
    case FrameTick =>
      if (gameState == GameState.GameRunning)
        updateRunningGame(gameTime, dice, shipControl, screenBounds)
      else
        Outcome(this)

    case KeyUp(Key.ESCAPE) =>
      Outcome(
        if (gameState == GameState.GameRunning) {
          val game = this.copy(gameState = GameState.GamePaused)
          if (game.verticalOffset >= targetVerticalOffset)
            game.copy(gameState = GameState.GameWin)
          else if (game.timeRemainingInSeconds <= 0)
            game.copy(gameState = GameState.GameLoss)
          else
            game
        } else if (gameState == GameState.GamePaused)
          this.copy(gameState = GameState.GameRunning)
        else
          this
      )

    case _ =>
      Outcome(this)
  }

  def buyUpgrade(upgrade: Upgrade) =
    if (timeRemainingInSeconds > upgrade.cost)
      this.copy(
        ship = ship.copy(health = ship.health + upgrade.healthBoost /*, speed = ship.speed + upgrade.speedBoost*/ ), // should be a function of Ship
        timeRemainingInSeconds =
          timeRemainingInSeconds - upgrade.cost
      )
    else
      this

  def spawnAsteroid(gameTime: GameTime, dice: Dice, screenBounds: BoundingBox) =
    if (gameTime.running.value < nextAsteroidSpawn || asteroids.length >= maxAsteroids)
      this
    else
      this.copy(
        asteroids =
          Asteroid.initial
            .moveTo(dice.rollDouble * screenBounds.width - 16, screenBounds.y - 20)
            .withRotation(dice.rollDouble * 360)
            .withRotationSpeed(dice.rollDouble)
            :: asteroids,
        nextAsteroidSpawn = gameTime.running.value + (dice.rollDouble * (maxAsteroidSpawnRate - minAsteroidSpawnRate) + minAsteroidSpawnRate)
      )

  def updateRunningGame(gameTime: GameTime, dice: Dice, shipControl: ShipControl, screenBounds: BoundingBox) = {
    val verticalDelta = verticalSpeed * 1 * gameTime.delta.value

    Outcome(
      this
        .copy(
          ship = ship
            .update(gameTime, asteroids.map(_.getBoundingBox), shipControl, screenBounds),
          asteroids = asteroids
            .map(_.moveBy(0, verticalDelta))
            .filter(a =>
              a.coords.x > screenBounds.x - a.boundingBox.width
                && a.coords.x < screenBounds.x + screenBounds.width
                && a.coords.y > screenBounds.y - a.boundingBox.height
                && a.coords.y < screenBounds.y + screenBounds.height
            ),
          // verticalOffset = verticalOffset + verticalDelta,
          timeRemainingInSeconds = Math.max(0, timeRemainingInSeconds - gameTime.delta.value)
        )
        .spawnAsteroid(gameTime, dice, screenBounds)
    )
  }
}

object Game {
  val maxTimeLimit: Double = 600 // 10 Minutes

  def initial(screenBounds: BoundingBox): Game =
    Game(GameState.GameRunning, Ship.initial(screenBounds), maxTimeLimit, Nil, 0, 0)
}

sealed trait GameState
object GameState {
  case object SplashScreen      extends GameState
  case object ShipCustomisation extends GameState
  case object GameWin           extends GameState
  case object GameLoss          extends GameState
  case object GameRunning       extends GameState
  case object GamePaused        extends GameState
}
