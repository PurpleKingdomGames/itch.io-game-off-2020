package com.moonshot.model

import indigo._
import indigo.shared.events.KeyboardEvent.KeyDown
import indigo.shared.events.KeyboardEvent.KeyUp
import indigo.shared.time.GameTime
import indigoextras.geometry.BoundingBox
import indigoextras.geometry.Vertex

final case class Game(ship: Ship, asteroids: List[Asteroid], verticalOffset: Double, nextAsteroidSpawn : Double) {
  val verticalSpeed : Double = 40
  val boundingBox : BoundingBox = BoundingBox(Vertex(0, 0), Vertex(350, 700))
  val maxAsteroids : Int = 20
  val minAsteroidSpawnRate : Double = 1
  val maxAsteroidSpawnRate : Double = 3

  def update(gameTime: GameTime, dice: Dice): GlobalEvent => Outcome[Game] = {
    case FrameTick => {
      val verticalDelta = (verticalSpeed * gameTime.delta.value)
      Outcome(this
        .copy(
          ship = ship
            .update(gameTime, asteroids.map(_.getBoundingBox()))
            .moveBy(0, (
              if (ship.health < 1)
                verticalDelta * 1.5
              else
                0
            ))
            .clampTo(boundingBox),
          asteroids = asteroids
            .map(_.moveBy(0, verticalDelta))
            .filter(a =>
              a.coords.x > boundingBox.x - a.boundingBox.width
              && a.coords.x < boundingBox.x + boundingBox.width
              && a.coords.y > boundingBox.y - a.boundingBox.height
              && a.coords.y < boundingBox.y + boundingBox.height
            ),
          verticalOffset = verticalOffset + verticalDelta
      )
      .spawnAsteroid(gameTime, dice))
    }


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

  def spawnAsteroid(gameTime : GameTime, dice : Dice) =
    if (gameTime.running.value < nextAsteroidSpawn || asteroids.length >= maxAsteroids)
      this
    else
      this.copy(
        asteroids =
            Asteroid
              .initial
              .moveTo(dice.rollDouble * boundingBox.width - 16, boundingBox.y - 20)
              .withRotation(dice.rollDouble * 360)
              .withRotationSpeed(dice.rollDouble)
            :: asteroids
        , nextAsteroidSpawn = gameTime.running.value + (dice.rollDouble * (maxAsteroidSpawnRate - minAsteroidSpawnRate) + minAsteroidSpawnRate)
      )
}

object Game {
  val initial: Game =
    Game(Ship.initial, Nil, 0, 0)
}

sealed trait GameState
object GameState {
  case object SplashScreen      extends GameState
  case object ShipCustomisation extends GameState
  case object GameRunning       extends GameState
  case object GamePaused        extends GameState
}
