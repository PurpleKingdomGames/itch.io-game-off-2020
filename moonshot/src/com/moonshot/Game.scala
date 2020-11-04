package com.moonshot

import indigo.GlobalEvent
import indigo.Outcome

final case class Game() {

  def update(): GlobalEvent => Outcome[Game] = {
    case _ =>
        Outcome(this)
  }
}

object Game {
  def initial(): Game =
    Game()
}

sealed trait GameState
object GameState {
  case object SplashScreen      extends GameState
  case object ShipCustomisation extends GameState
  case object GameRunning       extends GameState
  case object GamePaused        extends GameState
}