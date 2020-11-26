package com.moonshot.core

import indigo.shared.config.GameViewport
import indigo.shared.datatypes.Rectangle
import indigo.shared.dice.Dice

final case class StartUpData(magnification: Int, gameViewport: GameViewport, dice: Dice) {
  val initialScreenBounds: Rectangle =
    gameViewport.giveDimensions(magnification)
}
