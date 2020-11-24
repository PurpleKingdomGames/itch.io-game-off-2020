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
    nextAsteroidSpawn: Double,
    targetVerticalOffset: Double
) {
  val maxAsteroids: Int                  = 20
  val initMinAsteroidSpawnRate: Double   = 1
  val initMaxAsteroidSpawnRate: Double   = 0.5
  val targetMinAsteroidSpawnRate: Double = 0.1
  val targetMaxAsteroidSpawnRate: Double = 0.25

  def percentComplete: Double =
    Math.floor((100 * (verticalOffset / targetVerticalOffset)) * 100) / 100

  def withState(gameState: GameState) =
    this.copy(gameState = gameState)

  def update(gameTime: GameTime /*, dice: Dice*/, shipControl: ShipControl, screenBounds: BoundingBox): GlobalEvent => Outcome[Game] = {
    case FrameTick =>
      gameState match {
        case GameState.GameRunning if verticalOffset >= targetVerticalOffset =>
          Outcome(this.copy(gameState = GameState.GameWin))

        case GameState.GameRunning if timeRemainingInSeconds.toDouble <= 0 =>
          Outcome(this.copy(gameState = GameState.GameLoss))

        case GameState.GameRunning =>
          updateRunningGame(gameTime /*, dice*/, shipControl, screenBounds)

        case GameState.ShipCustomisation =>
          updateCustomisationScreen(gameTime, screenBounds)

        case _ =>
          Outcome(this)
      }

    case KeyUp(Key.ESCAPE) =>
      gameState match {
        case GameState.GameRunning if ship.lastDeath == Seconds.zero =>
          Outcome(this.copy(gameState = GameState.GamePaused))

        case GameState.GamePaused =>
          Outcome(this.copy(gameState = GameState.GameRunning))

        case _ =>
          Outcome(this)
      }

    case KeyUp(Key.ENTER) =>
      gameState match {
        case GameState.GameWin | GameState.GameLoss =>
          Outcome(Game.initial(screenBounds))

        case _ =>
          Outcome(this)
      }

    case _ =>
      Outcome(this)
  }

  // def buyUpgrade(upgrade: Upgrade) =
  //   if (timeRemainingInSeconds > upgrade.cost)
  //     this.copy(
  //       ship = ship.copy(health = ship.health + upgrade.healthBoost),
  //       timeRemainingInSeconds =
  //         timeRemainingInSeconds - upgrade.cost,
  //       initialSpeed = initialSpeed + upgrade.speedBoost
  //     )
  //   else
  //     this

  // def spawnAsteroid(gameTime: GameTime, dice: Dice, screenBounds: BoundingBox) =
  //   if (gameTime.running.value < nextAsteroidSpawn || asteroids.length >= maxAsteroids)
  //     this
  //   else {
  //     val percentThroughLevel = verticalOffset / targetVerticalOffset
  //     val minAsteroidSpawnRate =
  //       initMinAsteroidSpawnRate - (
  //         (targetMinAsteroidSpawnRate - initMinAsteroidSpawnRate) * percentThroughLevel
  //       )
  //     val maxAsteroidSpawnRate =
  //       initMaxAsteroidSpawnRate - (
  //         (targetMaxAsteroidSpawnRate - initMaxAsteroidSpawnRate) * percentThroughLevel
  //       )

  //     this.copy(
  //       asteroids =
  //         Asteroid.initial
  //           .moveTo(dice.rollDouble * screenBounds.width - 16, screenBounds.y - 20)
  //           .withRotation(dice.rollDouble * 360)
  //           .withRotationSpeed(dice.rollDouble)
  //           :: asteroids,
  //       nextAsteroidSpawn = gameTime.running.value + (dice.rollDouble * (maxAsteroidSpawnRate - minAsteroidSpawnRate) + minAsteroidSpawnRate)
  //     )
  //   }

  def updateRunningGame(gameTime: GameTime /*, dice: Dice*/, shipControl: ShipControl, screenBounds: BoundingBox) = {
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
                && a.coords.y < screenBounds.y + screenBounds.height
            ),
          verticalOffset = Math.min(targetVerticalOffset, verticalOffset + verticalDelta),
          timeRemainingInSeconds = Seconds(Math.max(0, (timeRemainingInSeconds - gameTime.delta).toDouble))
        )
      // .spawnAsteroid(gameTime, dice, screenBounds)
    )
  }

  def updateCustomisationScreen(gameTime: GameTime, screenBounds: BoundingBox) =
    Outcome(
      this.copy(
        ship = ship.update(gameTime, Nil, ShipControl.Idle, screenBounds)
      )
    )

  def presentTime: String = {
    val intSeconds            = timeRemainingInSeconds.toInt
    val minutes               = intSeconds / 60
    val seconds               = intSeconds % 60
    val minutesToShow: String = if (minutes < 10) "0" + minutes.toString else minutes.toString
    val secondsToShow: String = if (seconds < 10) "0" + seconds.toString else seconds.toString

    minutesToShow + ":" + secondsToShow
  }
}

object Game {
  val maxTimeLimit: Seconds       = Seconds(300) // 5 Minutes
  val targetVerticalSpeed: Double = 400
  val initialSpeed: Double        = 40

  def initial(screenBounds: BoundingBox): Game =
    Game(
      LevelType.Lander,
      GameState.GameRunning,
      Ship.initial(screenBounds),
      maxTimeLimit,
      Nil,
      initialSpeed,
      targetVerticalSpeed,
      0,
      0,
      Game.maxTimeLimit.toDouble * 2 * Game.initialSpeed
    )
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
