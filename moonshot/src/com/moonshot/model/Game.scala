package com.moonshot.model

import indigo._
import indigo.shared.events.KeyboardEvent.KeyUp
import indigo.shared.time.GameTime
import indigoextras.geometry.BoundingBox

final case class Game(
    levelType: LevelType,
    gameState: GameState,
    ship: Ship,
    timeRemainingInSeconds: Seconds,
    asteroids: List[Asteroid],
    initialSpeed: Double,
    targetVerticalSpeed: Double,
    verticalOffset: Double,
    nextAsteroidSpawn: Double
) {
  val maxAsteroids: Int                  = 20
  val initMinAsteroidSpawnRate: Double   = 1
  val initMaxAsteroidSpawnRate: Double   = 3
  val targetMinAsteroidSpawnRate: Double = 0.25
  val targetMaxAsteroidSpawnRate: Double = 0.5
  val targetVerticalOffset: Double       = Game.maxTimeLimit.toDouble * 2 * Game.initialSpeed

  def withState(gameState: GameState) =
    this.copy(gameState = gameState)

  def update(gameTime: GameTime, dice: Dice, shipControl: ShipControl, screenBounds: BoundingBox): GlobalEvent => Outcome[Game] = {
    case FrameTick =>
      if (gameState == GameState.GameRunning)
        if (verticalOffset >= targetVerticalOffset)
          Outcome(this.copy(gameState = GameState.GameWin))
        else if (timeRemainingInSeconds.toDouble <= 0)
          Outcome(this.copy(gameState = GameState.GameLoss))
        else
          updateRunningGame(gameTime, dice, shipControl, screenBounds)
      else if (gameState == GameState.ShipCustomisation)
        updateCustomisationScreen(gameTime, screenBounds)
      else
        Outcome(this)

    case KeyUp(Key.ESCAPE) =>
      Outcome(
        if (gameState == GameState.GameRunning && ship.lastDeath == Seconds.zero)
          this.copy(gameState = GameState.GamePaused)
        else if (gameState == GameState.GamePaused)
          this.copy(gameState = GameState.GameRunning)
        else
          this
      )

    case KeyUp(Key.ENTER) =>
      Outcome(
        if (gameState == GameState.GameWin || gameState == GameState.GameLoss)
          Game.initial(screenBounds)
        else
          this
      )

    case _ =>
      Outcome(this)
  }

  def buyUpgrade(upgrade: Upgrade) =
    if (timeRemainingInSeconds > upgrade.cost)
      this.copy(
        ship = ship.copy(health = ship.health + upgrade.healthBoost),
        timeRemainingInSeconds =
          timeRemainingInSeconds - upgrade.cost,
        initialSpeed = initialSpeed + upgrade.speedBoost
      )
    else
      this

  def spawnAsteroid(gameTime: GameTime, dice: Dice, screenBounds: BoundingBox) =
    if (gameTime.running.value < nextAsteroidSpawn || asteroids.length >= maxAsteroids)
      this
    else {
      val percentThroughLevel = verticalOffset / targetVerticalOffset
      val minAsteroidSpawnRate =
        initMinAsteroidSpawnRate - (
          (targetMinAsteroidSpawnRate - initMinAsteroidSpawnRate) * percentThroughLevel
        )
      val maxAsteroidSpawnRate =
        initMaxAsteroidSpawnRate - (
          (targetMaxAsteroidSpawnRate - initMaxAsteroidSpawnRate) * percentThroughLevel
        )

      this.copy(
        asteroids =
          Asteroid.initial
            .moveTo(dice.rollDouble * screenBounds.width - 16, screenBounds.y - 20)
            .withRotation(dice.rollDouble * 360)
            .withRotationSpeed(dice.rollDouble)
            :: asteroids,
        nextAsteroidSpawn = gameTime.running.value + (dice.rollDouble * (maxAsteroidSpawnRate - minAsteroidSpawnRate) + minAsteroidSpawnRate)
      )
    }

  def updateRunningGame(gameTime: GameTime, dice: Dice, shipControl: ShipControl, screenBounds: BoundingBox) = {
    val verticalSpeed = Math.max(initialSpeed, targetVerticalSpeed * (verticalOffset / targetVerticalOffset))
    val verticalDelta = -(Math.max(-3, Math.min(-1, ship.force.y)) * verticalSpeed) * gameTime.delta.value

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
          verticalOffset = Math.min(targetVerticalOffset, verticalOffset + verticalDelta),
          timeRemainingInSeconds = Seconds(Math.max(0, (timeRemainingInSeconds - gameTime.delta).toDouble))
        )
        .spawnAsteroid(gameTime, dice, screenBounds)
    )
  }

  def updateCustomisationScreen(gameTime: GameTime, screenBounds: BoundingBox) =
    Outcome(
      this
        .copy(
          ship = ship
            .update(gameTime, Nil, ShipControl.Idle, screenBounds)
        )
    )

  def presentTime: String = {
    val intSeconds = timeRemainingInSeconds.toInt
    val minutes    = intSeconds / 60
    val seconds    = intSeconds % 60

    (if (minutes < 10) "0" + minutes.toString else minutes.toString) +
      ":" +
      (if (seconds < 10) "0" + seconds.toString else seconds.toString)
  }
}

object Game {
  val maxTimeLimit: Seconds       = Seconds(300) // 5 Minutes
  val targetVerticalSpeed: Double = 400
  val initialSpeed: Double        = 40

  def initial(screenBounds: BoundingBox): Game =
    Game(LevelType.Lander, GameState.GameRunning, Ship.initial(screenBounds), maxTimeLimit, Nil, initialSpeed, targetVerticalSpeed, 0, 0)
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
