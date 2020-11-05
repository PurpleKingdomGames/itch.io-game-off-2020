package com.moonshot.model

import indigo._
import indigo.shared.events.KeyboardEvent.KeyDown
import indigo.shared.time.GameTime

final case class Game(ship: Ship) {

  def update(gameTime: GameTime): GlobalEvent => Outcome[Game] = {
    case KeyDown(k) =>
      k match {
        case Key.RIGHT_ARROW =>
          Outcome(this.copy(ship = ship.moveRight(gameTime)))

        case Key.LEFT_ARROW =>
          Outcome(this.copy(ship = ship.moveLeft(gameTime)))

        case Key.UP_ARROW =>
          Outcome(this.copy(ship = ship.moveUp(gameTime)))

        case Key.DOWN_ARROW =>
          Outcome(this.copy(ship = ship.moveDown(gameTime)))

        case _ =>
          Outcome(this)
      }
    case _ =>
      Outcome(this)
  }
}

object Game {
  val initial: Game =
    Game(Ship.initial)
}

sealed trait GameState
object GameState {
  case object SplashScreen      extends GameState
  case object ShipCustomisation extends GameState
  case object GameRunning       extends GameState
  case object GamePaused        extends GameState
}
