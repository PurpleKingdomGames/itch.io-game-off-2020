package com.moonshot.core

import indigo.shared.config.GameViewport
import indigo.shared.datatypes.Rectangle

final case class StartUpData(magnification: Int, gameViewport: GameViewport) {
  val initialScreenBounds: Rectangle =
    gameViewport.giveDimensions(magnification)
}
