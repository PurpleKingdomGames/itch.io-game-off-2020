package com.moonshot.model

import indigo._
import indigo.shared.events.KeyboardEvent.KeyDown
import indigo.shared.events.KeyboardEvent.KeyUp
import indigo.shared.time.GameTime
import indigoextras.geometry.BoundingBox
import indigoextras.geometry.Vertex

final case class Game(gameState: GameState, ship: Ship, timeRemainingInSeconds: Double, asteroids: List[Asteroid], verticalOffset: Double, nextAsteroidSpawn: Double) {
  val verticalSpeed: Double        = 40
  val boundingBox: BoundingBox     = BoundingBox(Vertex(0, 0), Vertex(350, 700))
  val maxAsteroids: Int            = 20
  val minAsteroidSpawnRate: Double = 1
  val maxAsteroidSpawnRate: Double = 3
  val targetVerticalOffset: Double = (Game.maxTimeLimit - 120) * verticalSpeed

  def update(gameTime: GameTime, dice: Dice): GlobalEvent => Outcome[Game] = {
    case FrameTick =>
      if (gameState == GameState.GameRunning)
        updateRunningGame(gameTime, dice)
      else
        Outcome(this)

    case KeyDown(k) =>
      k match {
        case Key.RIGHT_ARROW =>
          Outcome(this.copy(ship = ship.moveRight()))

        case Key.LEFT_ARROW =>
          Outcome(this.copy(ship = ship.moveLeft()))

        case Key.UP_ARROW =>
          Outcome(this.copy(ship = ship.moveUp()))

        case Key.DOWN_ARROW =>
          Outcome(this.copy(ship = ship.moveDown()))

        case Key.ESCAPE =>
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
    case KeyUp(k) =>
      k match {
        case Key.RIGHT_ARROW =>
          Outcome(this.copy(ship = ship.stopHorizontal()))

        case Key.LEFT_ARROW =>
          Outcome(this.copy(ship = ship.stopHorizontal()))

        case Key.UP_ARROW =>
          Outcome(this.copy(ship = ship.stopVertical()))

        case Key.DOWN_ARROW =>
          Outcome(this.copy(ship = ship.stopVertical()))

        case _ =>
          Outcome(this)
      }
    case _ =>
      Outcome(this)
  }

  def buyUpgrade(upgrade: Upgrade) =
    if (timeRemainingInSeconds > upgrade.cost)
      this.copy(
        ship = ship.copy(health = ship.health + upgrade.healthBoost, speed = ship.speed + upgrade.speedBoost),
        timeRemainingInSeconds =
          timeRemainingInSeconds - upgrade.cost
      )
    else
      this

  def spawnAsteroid(gameTime: GameTime, dice: Dice) =
    if (gameTime.running.value < nextAsteroidSpawn || asteroids.length >= maxAsteroids)
      this
    else
      this.copy(
        asteroids =
          Asteroid.initial
            .moveTo(dice.rollDouble * boundingBox.width - 16, boundingBox.y - 20)
            .withRotation(dice.rollDouble * 360)
            .withRotationSpeed(dice.rollDouble)
            :: asteroids,
        nextAsteroidSpawn = gameTime.running.value + (dice.rollDouble * (maxAsteroidSpawnRate - minAsteroidSpawnRate) + minAsteroidSpawnRate)
      )

  def updateRunningGame(gameTime: GameTime, dice: Dice) = {
    val verticalDelta = verticalSpeed * ship.speed * gameTime.delta.value
    Outcome(
      this
        .copy(
          ship = ship
            .update(gameTime, asteroids.map(_.getBoundingBox()))
            .moveBy(
              0,
              (
                if (ship.health < 1)
                  verticalDelta * 1.5
                else
                  0
              )
            )
            .clampTo(boundingBox),
          asteroids = asteroids
            .map(_.moveBy(0, verticalDelta))
            .filter(a =>
              a.coords.x > boundingBox.x - a.boundingBox.width
                && a.coords.x < boundingBox.x + boundingBox.width
                && a.coords.y > boundingBox.y - a.boundingBox.height
                && a.coords.y < boundingBox.y + boundingBox.height
            ),
          verticalOffset = verticalOffset + verticalDelta,
          timeRemainingInSeconds = Math.max(0, timeRemainingInSeconds - gameTime.delta.value)
        )
        .spawnAsteroid(gameTime, dice)
    )
  }
}

object Game {
  val maxTimeLimit: Double = 600 // 10 Minutes

  val initial: Game =
    Game(GameState.GameRunning, Ship.initial, maxTimeLimit, Nil, 0, 0)
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
