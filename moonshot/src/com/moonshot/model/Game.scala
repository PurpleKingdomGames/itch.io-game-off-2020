package com.moonshot.model

import indigo._
import indigo.shared.events.KeyboardEvent.KeyDown
import indigo.shared.events.KeyboardEvent.KeyUp
import indigo.shared.time.GameTime

final case class Game(ship: Ship, asteroids: List[Asteroid], verticalOffset: Double ) {
  val verticalSpeed : Double = 30;

  def update(gameTime: GameTime): GlobalEvent => Outcome[Game] = {
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
            )),
          asteroids = asteroids.map(_.moveBy(0, verticalDelta)),
          verticalOffset = verticalOffset + verticalDelta
      ))
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
}

object Game {
  val initial: Game =
    Game(Ship.initial, List(new Asteroid(Vector2(159, 64), 0, 10)), 0)
}

sealed trait GameState
object GameState {
  case object SplashScreen      extends GameState
  case object ShipCustomisation extends GameState
  case object GameRunning       extends GameState
  case object GamePaused        extends GameState
}
