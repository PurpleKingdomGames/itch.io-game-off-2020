package com.moonshot.model

import indigo.GlobalEvent
import indigo.Outcome
import indigo.shared.events.KeyboardEvent.KeyDown
import indigo.shared.time.GameTime

final case class Game(ship: Ship) {

  def update(gameTime: GameTime): GlobalEvent => Outcome[Game] = {
    case _ @ KeyDown (k) =>
        k.key match {
            case "ArrowRight" =>
                Outcome(this.copy(ship = ship.moveRight(gameTime)))

            case "ArrowLeft" =>
                Outcome(this.copy(ship = ship.moveLeft(gameTime)))

            case "ArrowUp" =>
                Outcome(this.copy(ship = ship.moveUp(gameTime)))

            case "ArrowDown" =>
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
