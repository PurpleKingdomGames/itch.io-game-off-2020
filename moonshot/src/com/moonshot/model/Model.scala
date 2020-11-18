package com.moonshot.model

import indigoextras.geometry.BoundingBox

final case class Model(game: Game)

object Model {

  def initial(screenBounds: BoundingBox): Model =
    Model(Game.initial(screenBounds))

}
