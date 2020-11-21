package com.moonshot.model

import indigoextras.geometry.BoundingBox
import com.moonshot.scenes.loading.LoadingModel

final case class Model(
    loadingScene: LoadingModel,
    game: Game
)

object Model {

  def initial(screenBounds: BoundingBox): Model =
    Model(
      LoadingModel.initial,
      Game.initial(screenBounds)
    )

}
