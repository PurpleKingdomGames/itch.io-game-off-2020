package com.moonshot.core

import indigoextras.geometry.BoundingBox
import indigo.shared.config.GameViewport

final case class StartUpData(screenBounds: BoundingBox, magnificaiton: Int, gameViewport: GameViewport)
